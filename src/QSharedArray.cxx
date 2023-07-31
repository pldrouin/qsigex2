// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#include "QSharedArray.h"

ClassImp(QSharedArray)

//#define FuncDef(a,b) const char* thefunc=#a; Bool_t show=b; const void* me=NULL;
//#define CFuncDef(a,b) const char* thefunc=#a; Bool_t show=b; const void* me=this;
//#define printstatus(a) {if(show) printf("%p\t%s\n",me,a);}
//#define pthread_mutex_lock(a) {if(show) printf("\n %p%slocking %s in function %s...\n",me,indent.Data(),#a,thefunc); pthread_mutex_lock(a); if(show) printf("%p %s%s is now locked in function %s\n",me,indent.Data(),#a,thefunc); indent+="     ";}
//#define pthread_mutex_unlock(a) {indent=indent(0,indent.Length()-5); if(show) printf("\n%p %sunlocking %s in function %s...\n",me,indent.Data(),#a,thefunc); pthread_mutex_unlock(a); if(show) printf("%p %s%s is now unlocked in function %s\n",me,indent.Data(),#a,thefunc);}
//#define pthread_cond_signal(a) {if(show) printf("%p sending signal %s in function %s...\n",me,#a,thefunc); pthread_cond_signal(a);}  
//#define pthread_cond_wait(a,b) {if(show) printf("%p waiting for signal %s with mutex %s in function %s...\n",me,#a,#b,thefunc); pthread_cond_wait(a,b);}
//#define ASSERT(a) if(!(a) && show) {fprintf(stderr,"Error in function %s: Assertion failed: %s\n",thefunc,#a); throw 1;}

#define FuncDef(a,b)
#define CFuncDef(a,b)
#define printstatus(a)
#define ASSERT(a)

void dummy_h(int sig){}

pthread_mutex_t QSharedArray::fIMutex=PTHREAD_MUTEX_INITIALIZER;
QSharedArray **QSharedArray::fInstances=NULL;
int QSharedArray::fNInstances=0;
//Base the alignment on the alignment for a double
const unsigned int QSharedArray::sSHPAMSize=(((sizeof(struct sharst)+sizeof(struct sharpars))/sysconf(_SC_PAGESIZE))+((sizeof(struct sharst)+sizeof(struct sharpars))%sysconf(_SC_PAGESIZE)!=0))*sysconf(_SC_PAGESIZE);

QSharedArray::QSharedArray(const char *filename, const char *arraydescr, const unsigned int &objectsize, const unsigned int &nobjectsperbuffer): fShArSt(NULL), fFilename(filename), fShPath(), fFDesc(0), fShAPath(), fFADesc(0), fOwns(false), fWIdx(-1), fArrayPars(NULL), fArrayName(), fTStamp(), fObjectSize(objectsize), fObjectTypeName(), fBuffer(NULL), fNOPerBuffer(nobjectsperbuffer), fMemPerBuffer(0), fNObjects(0), fBuffers(NULL), fNBuffers(0)
{
  CFuncDef(QSharedArray,1);
  string sbuf=arraydescr;
  int k=sbuf.find_last_of('/');

  if(k==(int)string::npos) fArrayName=sbuf;
  else {
    fArrayName=sbuf.substr(0,k);
    fObjectTypeName=sbuf.substr(k+1);
  }

  struct stat thestat;

  //Gets the file info. stat() follows symlinks
  if(stat(fFilename.c_str(),&thestat)) {
    fprintf(stderr,"Problem with file '%s':",fFilename.c_str());
    perror("stat");
    throw 1;
  }

  int i;
  char path[NAME_MAX];
  path[0]='/'; //Needs to start the shared memory ID using /
  char *sbuf1=path+1;
  int len=sizeof(thestat.st_dev);
  unsigned char *sbuf2=(unsigned char*)&thestat.st_dev;

  for(i=0; i<len; ++i) {
    sprintf(sbuf1,"%02X",sbuf2[i]);
    sbuf1+=2;
  }
  sbuf1[0]='_';
  ++sbuf1;

  len=sizeof(thestat.st_ino);
  sbuf2=(unsigned char*)&thestat.st_ino;

  for(i=0; i<len; ++i) {
    sprintf(sbuf1,"%02X",sbuf2[i]);
    sbuf1+=2;
  }
  sbuf1[0]=0;
  //ASSERTION: path contains '/[file_devID]_[file_inode]' 

  //Block all signals
  block_signals_once(throw 1);
  fShPath=path;
  sbuf1[0]='+';
  sbuf1[1]=0;
  fShAPath=path;
  //Unblock all signals
  unblock_signals_once(throw 1);

  pthread_mutex_lock(&fIMutex);

  if(!fNInstances) {
    //Add handler for SIGALRM
    sigset_t mset;
    sigemptyset(&mset);
    //Add SIGALRM to the set
    sigaddset(&mset,SIGALRM);
    //Block SIGALRM
    if(sigprocmask(SIG_BLOCK,&mset,NULL)) {
      perror("sigprocmask");
      throw 1;
    }

    if(signal(SIGALRM, dummy_h) == SIG_ERR) {
      fprintf(stderr,"Failed to install SIGALRM handler in parent process\n");
      throw 1;
    }
  }
  ++fNInstances;
  fInstances=(QSharedArray**)realloc(fInstances,fNInstances*sizeof(QSharedArray*));
  fInstances[fNInstances-1]=this;
  pthread_mutex_unlock(&fIMutex);

  InitShMem();
}

QSharedArray::~QSharedArray()
{
  CFuncDef(~QSharedArray,1);
  printstatus("QSharedArray::~QSharedArray()");

  int i;
  pthread_mutex_lock(&fIMutex);
  for(i=fNInstances-1; i>=0; --i) {
    
    if(fInstances[i]==this) {

      for(;i<fNInstances-1; ++i) fInstances[i]=fInstances[i+1];
      --fNInstances;

      if(fNInstances) {
	fInstances=(QSharedArray**)realloc(fInstances,fNInstances*sizeof(QSharedArray*));

      } else {
	free(fInstances);
	fInstances=NULL;
      }
      break;
    }
  }
  pthread_mutex_unlock(&fIMutex);
  ClearShMem();
  Terminate();
}

void QSharedArray::ClearAllShMem()
{
  pthread_mutex_lock(&fIMutex);
  for(int i=fNInstances-1; i>=0; --i) fInstances[i]->ClearShMem();
  pthread_mutex_unlock(&fIMutex);
}

void QSharedArray::ClearShMem()
{
  //Block Signals in case the function is called by a signal handler
  bool error=false;
  block_signals_once();
  int i,j;

  //If the shared memory was ready to be accessed
  if(fShArSt) {
    //Terminate();

    //If owned the shared memory but could not finish loading
    if(fOwns && q_load(&fShArSt->lstatus)<2) {
      printf("Owner sets failed status\n");
      q_store(&fShArSt->lstatus,(char)-1);

      for(i=0; i<MAX_WAITERS; ++i) if((j=q_load(&fShArSt->waiters[i]))) {
	//printf("Sending SIGALRM to process %i\n",j);
	kill(j,SIGALRM);
      }
    }

    //If has index in waiters list
    if(fWIdx!=-1) {
      //Erase it
      q_fetch_and_compare_and_set(&fShArSt->waiters[fWIdx],(uint32_t)getpid(),(uint32_t)0);
      fWIdx=-1;
    }

retry2:
    //If last user
    if((i=q_fetch_and_compare_and_set(&fShArSt->susers,(int32_t)1,(int32_t)0))==1) {
      //printf("Destroying the shared memory space\n");

      if(shm_unlink(fShPath.c_str())==-1){
	perror("shm_unlink");
	error=true;
      }

      if(shm_unlink(fShAPath.c_str())==-1){
	perror("shm_unlink");
	error=true;
      }

      //If not last user 
    } else {

      //Try decrement if value has not changed. If value has changed and was 1, goto retry2. Otherwise retry
      while((j=q_fetch_and_compare_and_set(&fShArSt->susers,(int32_t)i,(int32_t)(i-1)))!=i) {if(j==1) goto retry2; i=j;}
      //printf("Decremented the shared memory usage to %i\n",j-1);
    }

    //if(munmap(fShArSt,sSHPAMSize)) {
    //  perror("munmap");
    //}
    fShArSt=NULL;

    //Else of owner but could not get shared memory
  } else if(fOwns) {

    if(shm_unlink(fShPath.c_str())==-1){
      perror("shm_unlink");
      error=true;
    }
  }

  if(fFDesc>0) {

    if(close(fFDesc)) {
      fFDesc=0;
      perror("close");
      error=true;
    }
    fFDesc=0;
  }

  if(fFADesc>0) {

    if(close(fFADesc)) {
      fFADesc=0;
      perror("close");
      error=true;
    }
    fFADesc=0;
  }
  unblock_signals_once();

  if(error) throw 1;
}

void QSharedArray::InitShMem()
{
  ClearShMem();
  //Add handler for SIGALRM
  sigset_t mset;
  sigemptyset(&mset);
  //Add SIGALRM to the set
  sigaddset(&mset,SIGALRM);

  int i,j;
  fOwns=false;

  block_signals_init(nset,oset,throw 1);

  //Block all signals
  block_signals(&nset,&oset,throw 1);

retry:
  //printf("Filename is '%s'\n",fFilename.c_str());
  //printf("Path is '%s'\n",fShPath.c_str());
  fFDesc=shm_open(fShPath.c_str(),O_RDWR,S_IRUSR|S_IWUSR);

  if(fFDesc==-1 && errno==ENOENT) {
    //printf("Owns the shared segment\n");
    fOwns=true;
    fFDesc=shm_open(fShPath.c_str(),O_RDWR|O_CREAT|O_EXCL,S_IRUSR|S_IWUSR);

    if(fFDesc==-1 && errno==EEXIST) {
      printf("Does not own the shared segment anymore\n");
      fOwns=false;
      goto retry;
    }
  }

  if(fFDesc==-1) {
    //printf("%s:\n",fFilename.c_str());
    perror("shm_open");
    //Stop blocking signals.
    unblock_signals(&oset,);
    throw 1;
  }

  if(ftruncate(fFDesc, sSHPAMSize)==-1) { 
    perror("ftruncate");
    //Stop blocking signals. Need to call terminatesmem to just handle the fFDesc
    unblock_signals(&oset,);
    throw 1;
  }

  fShArSt=(struct sharst*)mmap(NULL,sSHPAMSize,PROT_READ|PROT_WRITE,MAP_SHARED,fFDesc,0);

  if(fShArSt==MAP_FAILED) {
    perror("mmap");
    //Stop blocking signals. Need to call terminatesmem to handle the fFDesc and the mmap
    sigprocmask(SIG_SETMASK,&oset,NULL);
    throw 1;
  }

  //q_store(&fShArSt->susers,(int32_t)0);
  //fOwns=true;
  q_add(&fShArSt->susers,(int32_t)1);
  //printf("pid is %u\n",getpid());
  //printf("nusers is %i\n",q_load(&fShArSt->susers));

  //Knows the ownership, so safe to unblock signals
  unblock_signals(&oset,throw 1);

  //If creator of the shared memory
  if(fOwns) {
    //The owner indicates it is ready to start loading
    q_store(&fShArSt->lstatus,(char)1);
newowner:

    //printf("Loading the data...\n");

    //sleep(20);

    LoadArray();

    //printf("Done Loading\n");

    //The owner indicates it is done loading
    //q_store(&fShArSt->lstatus,(char)(k=int(2+rand()*120./RAND_MAX)));
    q_store(&fShArSt->lstatus,(char)2);
    //printf("Status is %i\n",k);
    //Sends a signal to all waiters
    for(i=0; i<MAX_WAITERS; ++i) if((j=q_load(&fShArSt->waiters[i]))) {
      //printf("Sending SIGALRM to process %i (%i)\n",j,k);
      kill(j,SIGALRM);
    }

  } else {
    i=-1;

    //Wait for the owner to change the status
    //printf("Trying to read the status\n");
    while(!q_load(&fShArSt->lstatus) && ++i<1000) {
      //printf("Sleep %i\n",i);
      usleep(1000);
    }

    if(i>=1000) {
      printf("Owner failed to initialize\n");

      ClearShMem();
      InitShMem();
      return;
    }

    pid_t pid=getpid();
    struct timespec timeout={60,0};
    int sig;

    //If owner is not done loading
    if(q_load(&fShArSt->lstatus)<2) {

      for(;;) {

	//Try to find a spot in the waiters' list and add pid
	if(fWIdx==-1) {
	  //Block signals to make sure fWIdx is set if an iterm is added to the waiters list
	  block_signals(&nset,&oset,throw 1);

	  for(i=0; i<MAX_WAITERS; ++i)
	    if(!q_fetch_and_compare_and_set(&fShArSt->waiters[i],(uint32_t)0,(uint32_t)pid)) {
	      fWIdx=i;
	      break;
	    }
	  unblock_signals(&oset,throw 1);
	}

	//If a spot has been found
	if(fWIdx>=0) {

	  //If the owner is still not done loading
	  if(q_load(&fShArSt->lstatus)==1) {

	    //printf("Waiting for Signal\n");
	    //If sigtimedwait generated an error
	    if((sig=sigtimedwait(&mset,NULL,&timeout))<0 && errno!=EAGAIN) {
	      perror("sigtimedwait");
	      throw 1;

	      //If no error from sigtimedwait
	    } else {

	      //Timed out!
	      if(errno==EAGAIN) {
		//printf("sigtimedwait received no signal\n");

		//Else if SIGALRM has been received and the owner is done loading
	      } else if(q_load(&fShArSt->lstatus)>=2) {
		//printf("sigtimedwait received SIGALRM and owner is done loading (%i)\n",i);
		q_store(&fShArSt->waiters[fWIdx],(uint32_t)0);
		fWIdx=-1;
		break;

	      } else {
		//printf("sigtimedwait received SIGALRM but owner is not done loading\n");
	      }
	    }

	    //Else if the owner is done loading
	  } else if(q_load(&fShArSt->lstatus)>=2) {
	    q_store(&fShArSt->waiters[fWIdx],(uint32_t)0);
	    fWIdx=-1;
	    break;
	  }

	  //If Could not find a spot, use a simple sleep
	} else {
	  usleep(1000);
	  //If owner is done loading
	  if(q_load(&fShArSt->lstatus)>=2) break;
	}

	//If owner has terminated
	if(q_fetch_and_compare_and_set(&fShArSt->lstatus,(char)-1,(char)1)==-1) {
	  //printf("Acquiring ownership\n");
	  q_store(&fShArSt->waiters[fWIdx],(uint32_t)0);
	  fWIdx=-1;
	  fOwns=true;
	  goto newowner;
	}
      }
    }
    //printf("Loading the data...\n");
    LoadArray();
    //printf("Done Loading\n");
  }
}

void QSharedArray::Fill()
{
  CFuncDef(Fill,1);

  fprintf(stderr,"Error: Fill: QSharedArray are read-only objects\n");
  throw 1;
}

void QSharedArray::LoadArray()
{
  fArrayPars=(struct sharpars*)((char*)fShArSt+sizeof(struct sharst));
  int fd=0;

  if(fOwns) {
#ifdef __FreeBSD__
    fd=open(fFilename.c_str(),O_RDONLY|O_DIRECT);
#else
    fd=open(fFilename.c_str(),O_RDONLY);
#endif

    if(fd<0) {
      perror("QSharedArray::LoadArray()::open");
      throw 1;
    }

    //Load Header
    if(read(fd, fArrayPars, sizeof(struct sharpars))!=sizeof(struct sharpars)) {
      perror("QSharedArray::LoadArray()::read");
      throw 1;
    }
  }

  if(strdiffer(fArrayPars->arrayname,fArrayName.c_str())) {
    fprintf(stderr,"QSharedArray::LoadArray: Error: Array name saved in file '%s' does not match provided array name\n",fFilename.c_str());
    throw 1;
  }

  uint32_t uibuf=strlen(fArrayPars->arrayname)+1;

  if(fObjectTypeName.size() && fArrayPars->anamelength>uibuf && strdiffer(fObjectTypeName.c_str(),fArrayPars->arrayname+uibuf)) {
    fprintf(stderr,"QSharedArray::LoadArray: Error: Array type '%s' saved in file '%s' does not match the provided array type ('%s')\n",fArrayPars->arrayname+uibuf,fFilename.c_str(),fObjectTypeName.c_str());
    throw 1;
  }

  if(!fObjectTypeName.size() && fArrayPars->anamelength>uibuf) fObjectTypeName=fArrayPars->arrayname+uibuf;

  if(fObjectSize!=0 && fArrayPars->objectsize!=fObjectSize) {
    fprintf(stderr,"QSharedArray::LoadArray(): Error: Objects size saved in file '%s' does not match provided objects size\n",fFilename.c_str());
    throw 1;
  }
  fObjectSize=fArrayPars->objectsize;

  if(!fNOPerBuffer) fNOPerBuffer=fArrayPars->noperbuffer;
  fNObjects=fArrayPars->nobjects;
  fTStamp.SetSec(fArrayPars->sec);
  fTStamp.SetNanoSec(fArrayPars->nsec);

  fMemPerBuffer=fNOPerBuffer*fObjectSize;
  fMemPerBuffer=(fMemPerBuffer/sysconf(_SC_PAGESIZE)+(fMemPerBuffer%sysconf(_SC_PAGESIZE)!=0))*sysconf(_SC_PAGESIZE);
  fNOPerBuffer=fMemPerBuffer/fObjectSize;
  //Done Loading header

  fNBuffers=(fNObjects/fNOPerBuffer+(fNObjects%fNOPerBuffer!=0));
  fBuffers=(void**)malloc(fNBuffers*sizeof(void*));

  unsigned int i;

  if(fOwns) {
    fFADesc=shm_open(fShAPath.c_str(),O_RDWR|O_CREAT,S_IRUSR|S_IWUSR);

    if(fFADesc==-1) {
      printf("%s+:\n",fFilename.c_str());
      perror("shm_open");
      throw 1;
    }

    if(ftruncate(fFADesc, fNBuffers*fMemPerBuffer)==-1) {
      perror("ftruncate");
      throw 1;
    }

    //Allocate shared memory for the whole array
    for(i=0; i<fNBuffers; ++i) {
      fBuffers[i]=mmap(NULL,fMemPerBuffer,PROT_READ|PROT_WRITE,MAP_SHARED,fFADesc,i*fMemPerBuffer);

      if(fBuffers[i]==MAP_FAILED) {
	perror("mmap");
	throw 1;
      }
    }
    unsigned int maxdbdatasize=fArrayPars->objectsize*fArrayPars->noperbuffer; //maximum disk buffer size
    unsigned int ndb=fNObjects/fArrayPars->noperbuffer;
    unsigned int wbidx=ndb; //Inaccessible index by default
    void *dbuf=malloc(maxdbdatasize); //disk buffer
    void *uzbuf=malloc(maxdbdatasize); //unzipped buffer
    void *rbuf;                        //readbuffer
    unsigned int bidx=0; //buffer index
    unsigned int midx=0; //buffer memory index
    int ibuf,ibuf2;
    unsigned int dmidx=0; //disk buffer memory index

    //If write buffer exists
    if(fNObjects%maxdbdatasize) ++ndb;

    for(i=0; i<ndb; ++i) {

      //Read buffer size
      if(read(fd, &uibuf, sizeof(uint32_t))!=sizeof(uint32_t)) {
	perror("QSharedArray::LoadArray()::read");
	throw 1;
      }
      //Read buffer 
      if(read(fd, dbuf, uibuf)!=(int)uibuf) {
	perror("QSharedArray::LoadArray()::read");
	throw 1;
      }

      if(midx==fMemPerBuffer) {
	++bidx;
	midx=0;
      }

      //If the disk buffer is compressed
      if(uibuf<maxdbdatasize && i!=wbidx) {
	//Unzip it
	R__unzip((int*)&uibuf,(unsigned char*)dbuf,(int*)&maxdbdatasize,(unsigned char*)uzbuf,&ibuf);

	if(!ibuf) {
	  fprintf(stderr,"QSharedArray::LoadArray: Error: A buffer cannot be unzipped\n");
	  throw 1;
	}
	rbuf=uzbuf;

	//Else if the disk buffer is not compressed
      } else {
	rbuf=dbuf;
	ibuf=uibuf;
      }

      dmidx=0;

      //While dbuf is not fully loaded into the shared memory
      for(;;) {

	//If there is enough room in the memory buffer to contain the data from the whole disk buffer
	if(fMemPerBuffer>=ibuf+midx) {
	  memcpy((char*)fBuffers[bidx]+midx,(char*)rbuf+dmidx,ibuf);
	  midx+=ibuf;
	  break;

	  //Else if it does not all fit
	} else {
	  ibuf2=(int)fMemPerBuffer-midx; //parts that fits
	  memcpy((char*)fBuffers[bidx]+midx,(char*)rbuf+dmidx,ibuf2);
	  midx=0;
	  ++bidx;
	  ibuf-=ibuf2;
	  dmidx+=ibuf2;
	}
      }
    }
    free(dbuf);
    free(uzbuf);

    //Protect memory
    for(i=0; i<fNBuffers; ++i) if(mprotect(fBuffers[i],fMemPerBuffer,PROT_READ)) {
      perror("mprotect");
      throw 1;
    }

#ifdef __linux__
    posix_fadvise(fd,0,0,POSIX_FADV_DONTNEED);
#endif

    if(close(fd)) {
      perror("close");
      throw 1;
    }

    //Use memory fence to ensure cache consistency
    q_mfence();

    //Ese if not owner
  } else {
    fFADesc=shm_open(fShAPath.c_str(),O_RDONLY,S_IRUSR|S_IWUSR);

    if(fFADesc==-1) {
      printf("%s+:\n",fFilename.c_str());
      perror("shm_open");
      throw 1;
    }

    //Allocate shared memory for the whole array
    for(i=0; i<fNBuffers; ++i) {
      fBuffers[i]=mmap(NULL,fMemPerBuffer,PROT_READ,MAP_SHARED,fFADesc,i*fMemPerBuffer);

      if(fBuffers[i]==MAP_FAILED) {
	perror("mmap");
	throw 1;
      }
    }
  }
}

void QSharedArray::LoadEntry(const long long int &entry)
{
  //Do not need to apply locks for reading operations of variables that are only modified in the main thread
  CFuncDef(LoadEntry,1);
#ifndef QSFAST
  //If the entry is out of bound
  if(entry<0 || entry>=fNObjects) {
    fprintf(stderr,"QSharedArray::LoadEntry: Error: Entry index is invalid\n");
    //printstatus("Exiting LoadEntry");
    return;
  }
#endif

  unsigned int lLEibuf=entry/fNOPerBuffer;

  memcpy(fBuffer,(char*)fBuffers[lLEibuf]+(entry-lLEibuf*fNOPerBuffer)*fObjectSize,fObjectSize);
  return;
}

void QSharedArray::PrintInfo() const
{
  printf("Array name: %s\n",fArrayName.c_str());
  if(fObjectTypeName.size()) printf("Array type name: %s\n",fObjectTypeName.c_str());
  printf("Objects size: %i\n",fObjectSize);
  printf("Number of objects per buffer: %u\n",fNOPerBuffer);
  printf("Total number of objects: %lli\n",fNObjects);
}

void QSharedArray::ShowMemStats()
{
#ifdef WITH_LIBPROCINFO
  printf("Available memory: %lli\n",sysfreemem());
#endif
}

void QSharedArray::Terminate()
{
  CFuncDef(Terminate,1);
  printstatus("void QSharedArray::Terminate()");
  fNObjects=0;

  if(fBuffers) { //Should check fBuffers since it garanties the memory was allocated

    for(unsigned int i=0; i<fNBuffers; ++i) {

      if(munmap(fBuffers[i],fMemPerBuffer)) {
	perror("munmap");
      }
    }
    free(fBuffers);
    fNBuffers=0;
    fBuffers=NULL;
    fMemPerBuffer=0;
  }
}

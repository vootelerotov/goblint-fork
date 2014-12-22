#define nproc 10
mtype = { NOTCREATED, STOPPED, SUSPENDED, WAITING, READY, RUNNING, DONE } // possible process states
mtype status[nproc] = NOTCREATED; // initialize all processes as not created
byte tmp;

inline doA() {
    select (tmp : 0..4);
    status[tmp] = RUNNING;
}

inline doB() {
    select (tmp : 5..9);
    status[tmp] = WAITING;
}

inline doStuff() { atomic {
    status[tmp] = STOPPED;
    if
    :: doA();
    :: doB();
    fi
    status[tmp] = DONE;
    /* if */
    /* :: doA(); */
    /* :: doB(); */
    /* fi */
    /* if */
    /* :: doA(); */
    /* :: doB(); */
    /* fi */
    /* if */
    /* :: doA(); */
    /* :: doB(); */
    /* fi */
} }

active proctype a() {
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
    doStuff();
}
#ifndef JSADDLE_H
#define JSADDLE_H

typedef struct native_callbacks {
  void (* jsaddleStart) ();
  void (* jsaddleResult) (char *);
  char * (* jsaddleSyncResult) (char *);
  char * jsaddleJsData;
  char * jsaddleHtmlData;
} native_callbacks;

typedef struct app_callbacks {
  void (* mainActivity_onCreate) ();
  void (* mainActivity_onStart) ();
  void (* mainActivity_onResume) ();
  void (* mainActivity_onPause) ();
  void (* mainActivity_onStop) ();
  void (* mainActivity_onDestroy) ();
  void (* mainActivity_onRestart) ();
  void (* mainActivity_onNewIntent) (char *, char *);
  void (* firebaseInstanceIdService_sendRegistrationToServer) (char *);
} app_callbacks;

#endif

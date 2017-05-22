#ifndef JSADDLE_H
#define JSADDLE_H

typedef struct native_callbacks {
  void (* jsaddleStart) ();
  void (* jsaddleResult) (char *);
  char * jsaddleJsData;
  char * jsaddleHtmlData;
} native_callbacks;

typedef struct app_callbacks {
  void (* firebaseInstanceIdService_sendRegistrationToServer) (char *);
} app_callbacks;

#endif

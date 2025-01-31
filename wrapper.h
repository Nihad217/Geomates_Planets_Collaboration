#include <box2d/box2d.h>

#ifndef __WRAPPER_H__
#define __WRAPPER_H__

#ifdef _WIN32
  #ifdef BUILDING_DLL
    #define DLLEXPORT __declspec(dllexport)
  #else
    #define DLLEXPORT __declspec(dllimport)
  #endif
#else
  #define DLLEXPORT
#endif

typedef struct bodyElem {
    b2BodyId b2Body;
    int ref;
    void *next;
} bodyElem;

// for querying pose of player object
typedef struct bodyPose {
    b2Vec2 position;
    float rotation; // for rect only
    float width; // rect only
    float height; // rect only
} bodyPose;

typedef struct growObstacles {
    b2Vec2 pos; // position of rect that is about to grow
    int left, right, bottom, top; // count for objects in directions
} growObstacles;

DLLEXPORT void initWorld(float gx, float gy);
DLLEXPORT void destroyWorld(void);
DLLEXPORT void initPlayers(float rx, float ry, float rs, float ratio, float rd, float rf, float dx, float dy, float ds, float dd, float df);
DLLEXPORT void worldInsertPlatform(float px, float py, float sx, float sy);
DLLEXPORT void stepWorld(void);
DLLEXPORT bodyPose* getDiscPlayerPose(void);
DLLEXPORT bodyPose* getRectPlayerPose(void);
DLLEXPORT void moveDiscPlayer(float f);
DLLEXPORT void jumpDiscPlayer(float f);
DLLEXPORT void transformRectPlayer(float s); // grow/shrink
DLLEXPORT void moveRectPlayer(float f); // move left/right
DLLEXPORT int bodyOnGround(b2BodyId body); // checks if body rests on some object
DLLEXPORT int pointInRectPlayer(float x, float y);
#endif

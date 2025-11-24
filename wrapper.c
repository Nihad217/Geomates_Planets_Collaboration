// This is a wrapper for box2d, allowing it to be used as
// dynamic library. Only those functions are currently supported
// that we need for our 'geomates' competition.

#define _USE_MATH_DEFINES
#include <math.h>
#include "wrapper.h"
#include <stdlib.h>
#include <stdio.h>

#define MAX_CONTACTS 32


b2WorldDef gWorldDef;   // global reference to the simulated world
b2WorldId gWorldID;
b2BodyId gDiscPlayer;   // global reference to disc player object
b2ShapeId gDiscShapeID; // ...and its shape
b2BodyId gRectPlayer;   // global reference to rect player object
b2ShapeId gRectShapeID; // ...and its shape
bodyElem *gBodies;      // list of created objects for clean-up
b2ContactData gContacts[MAX_CONTACTS]; // tmp storage
float gRectRatio = 1.0f; // global parameter width/height ratio of rect
float gRectSize;        // global parameter size of rect object

bodyPose tmpPose;       // tmp storage
growObstacles obstacles; // used in callback for growing the rect

// initialize global world structure
void initWorld(float gx, float gy) {
    b2Vec2 gravity;
    gWorldDef = b2DefaultWorldDef();
    gravity.x = gx;
    gravity.y = gy;
    gWorldDef.gravity = gravity;
    gWorldID = b2CreateWorld(&gWorldDef);
    gBodies = NULL;
}

// destroy global world structure
void destroyWorld(void) {
    // free storage for all bodies in list
    bodyElem *nxt = gBodies;
    while (nxt) {
        bodyElem *tmp = nxt->next;
        free(nxt);
        nxt = tmp;
    }
    gBodies = NULL;
    // cleanup by box2d
    b2DestroyWorld(gWorldID);
}

// computes width and height of rect player for given ratio and size
void setRectWidthHeight(float *boxW, float *boxH) {
    float wR = (gRectRatio > 0) ? gRectRatio : -1.0f/gRectRatio;
    *boxW = gRectSize * wR;
    *boxH = gRectSize / wR;
}

// initializes rect and disc player, then adding them to the world
void initPlayers(float rx, float ry, float rs, float ratio, float rd, float rf, float dx, float dy, float ds, float dd, float df) {
    // initialize the rect player
    b2BodyDef rectBodyDef = b2DefaultBodyDef();
    rectBodyDef.type = b2_dynamicBody;
    b2Vec2 position;
    position.x = rx;
    position.y = ry;
    rectBodyDef.position = position;
    
    gRectPlayer = b2CreateBody(gWorldID, &rectBodyDef);
    if (ratio>-1.0 && ratio < 1.0) ratio = 1.0; // safety first
    gRectRatio = ratio; // {-10, -9, ..., -2, 1, ..., 10}: >0: width=ratio*height, else: width=height/(-ratio);
    gRectSize  = rs;  // width=height of rect with ratio 1
    float boxW, boxH;
    setRectWidthHeight(&boxW, &boxH);
    b2Polygon dBox = b2MakeBox(boxW, boxH);
    b2ShapeDef rectShape = b2DefaultShapeDef();
    rectShape.density = rd;
    rectShape.material.friction = rf;
    gRectShapeID = b2CreatePolygonShape(gRectPlayer, &rectShape, &dBox);
    
    // initialize the disc player
    b2BodyDef discBodyDef = b2DefaultBodyDef();
    discBodyDef.type = b2_dynamicBody;
    position.x = dx;
    position.y = dy;
    discBodyDef.position = position;
    
    gDiscPlayer = b2CreateBody(gWorldID, &discBodyDef);
    b2Circle disc;
    disc.radius = ds;
    disc.center.x = 0;
    disc.center.y = 0;
    b2ShapeDef discShapeDef = b2DefaultShapeDef();
    discShapeDef.density = dd;
    discShapeDef.material.friction = df;
    gDiscShapeID = b2CreateCircleShape(gDiscPlayer, &discShapeDef, &disc);
}

void worldInsertDynamicBox(unsigned int idx, float posx, float posy, float sx, float sy, float density, float friction) {
    b2BodyDef bodyDef = b2DefaultBodyDef();
    bodyDef.type = b2_dynamicBody;
    b2Vec2 position;
    position.x = posx;
    position.y = posy;
    bodyDef.position = position;
    
    bodyElem *newBody = malloc(sizeof(bodyElem));
    if (!newBody) exit(-1); // FIXME: better error handling
    
    newBody->b2Body = b2CreateBody(gWorldID, &bodyDef);
    newBody->next = gBodies; // insert body into global list
    gBodies = newBody;
    b2Polygon dBox = b2MakeBox(sx,sy);
    b2ShapeDef shapeDef = b2DefaultShapeDef();
    shapeDef.density = density;
    shapeDef.material.friction = friction;
    b2CreatePolygonShape(newBody->b2Body, &shapeDef, &dBox);
}

// inserts an unmovable axis-aligned box with coordinates (px,py), (sx,sy)
void worldInsertPlatform(float px, float py, float sx, float sy) {
    b2BodyDef bodyDef = b2DefaultBodyDef();
    bodyDef.type = b2_staticBody;
    b2Vec2 position;
    position.x = 0.5f*(px+sx);
    position.y = 0.5f*(py+sy);
    bodyDef.position = position;

    b2BodyId platformId = b2CreateBody(gWorldID, &bodyDef);

    b2Polygon dBox = b2MakeBox(0.5f*fabsf(sx-px),0.5f*fabsf(sy-py));
    b2ShapeDef shapeDef = b2DefaultShapeDef();
    shapeDef.density = 1.0f;
    shapeDef.material.friction = 0.3f;
    b2CreatePolygonShape(platformId, &shapeDef, &dBox);
}

// not used
int diamondHit(float dx, float dy) {
    return 0;
    
}

//
// all about game control and simulation
//

// advances the world by one tick
void stepWorld(void) {
    b2World_Step(gWorldID, 1.0f / 60.0f, 4);
}

/*
// search through list of bodies and return pose of requested body
// FIXME: linear search is bad
bodyPose* getPose(unsigned int idx) {
    bodyElem *elem = gBodies;
    while (elem) {
        if (elem->ref == idx) {
            tmpPose.position = b2Body_GetPosition(elem->b2Body);
            tmpPose.rotation = b2Rot_GetAngle(b2Body_GetRotation(elem->b2Body));
        } else elem = elem->next;
    }
    return &tmpPose; // FIXME: signal error if not found
}
*/

bodyPose* getRectPlayerPose(void) {
    tmpPose.position = b2Body_GetPosition(gRectPlayer);
    tmpPose.rotation = b2Rot_GetAngle(b2Body_GetRotation(gRectPlayer));
    setRectWidthHeight(&tmpPose.width, &tmpPose.height);
    tmpPose.width *= 2.0f;  // radius -> width
    tmpPose.height *= 2.0f;
    return &tmpPose;
}

bodyPose* getDiscPlayerPose(void) {
    tmpPose.position = b2Body_GetPosition(gDiscPlayer);
    tmpPose.rotation = b2Rot_GetAngle(b2Body_GetRotation(gDiscPlayer));
    return &tmpPose;
}


// returns 1 if non-disc object is in contact in a direction
// directions are given as Box2d's contact normals
int obstacleInDirection(b2BodyId body, float x, float y) {
    // retrieve contacts of disc
    int cs = b2Body_GetContactData(body, gContacts, MAX_CONTACTS);
    // search for contact 'above', i.e., with downward pointing normal
    for (int i=0; i<cs; i++) {
        for (int j=0; j<gContacts[i].manifold.pointCount; j++) {
            float s = gContacts[i].manifold.normal.x * x + gContacts[i].manifold.normal.y * y;
            if (s>0.3f) {
                // contact found, compare to disc shape
                if (B2_ID_EQUALS(gContacts[i].shapeIdA, gDiscShapeID)) return 1;
                if (B2_ID_EQUALS(gContacts[i].shapeIdB, gDiscShapeID)) return 1;
            }
        }
    }
    return 0;
}


//
// rect player
//

// returns 1 if body is in contact with another body below, 0 otherwise
// something fishy is going on here: the normal seems to flip from -y to +1
// around a rotation angle of +/- 0.0 but then sticks with that polarity
// according to the docs, the normal should be in world coordinates
// for time being, we don't distinguish top and bottom contacts
int rectOnGround(void) {
    int cs = b2Body_GetContactData(gRectPlayer, gContacts, MAX_CONTACTS);
    // search for contact 'below', i.e., with upward pointing normal
    //float rot = b2Rot_GetAngle(b2Body_GetRotation(gRectPlayer));
    for (int i=0; i<cs; i++) {
        for (int j=0; j<gContacts[i].manifold.pointCount; j++) {
            //printf("normal = %f/%f rotation = %f\n", gContacts[i].manifold.normal.x, gContacts[i].manifold.normal.y, rot);
            if (fabsf(gContacts[i].manifold.normal.y)>0.7) return 1;
            /*
            if (fabsf(rot)<0.7854f && gContacts[i].manifold.normal.y<-0.7) {
                return 1;
            } else if (gContacts[i].manifold.normal.y>0.7) {
                return 1;
            }
             */
        }
    }
    return 0;
}


// move left/right
void moveRectPlayer(float f) {
    //
    //if (rectOnGround()) {
        float boxW, boxH;
        setRectWidthHeight(&boxW, &boxH);
        tmpPose.position = b2Body_GetPosition(gRectPlayer);
        tmpPose.position.y -= 0.5f*boxH;
        b2Vec2 impulse;
        impulse.x = f;
        impulse.y = 0.0f;
        //b2Body_ApplyLinearImpulseToCenter(gRectPlayer, impulse, 1);
        b2Body_ApplyLinearImpulse(gRectPlayer, impulse, tmpPose.position, 1);
    //}
}

int inside(float x, float a, float b) {
    return a <= x && x <= b;
}

// callback for collison test
// counts objects in contact from left, right, top, and bottom
bool rectTransformCheck(b2ShapeId shapeId, growObstacles *obstacles) {
    /*
    b2AABB bb2 = b2Shape_GetAABB(shapeId);
    printf("(%f,%f)--(%f,%f)", bb2.lowerBound.x, bb2.lowerBound.y, bb2.upperBound.x, bb2.upperBound.y);
    if (B2_ID_EQUALS(shapeId, gDiscShapeID)) printf(" disc");
    if (B2_ID_EQUALS(shapeId, gRectShapeID)) printf(" rect");
    printf("\n");
     */

    if (!(B2_ID_EQUALS(shapeId, gRectShapeID)) && !B2_ID_EQUALS(shapeId, gDiscShapeID)) {
        b2AABB bb = b2Shape_GetAABB(shapeId);
        if (bb.upperBound.x < obstacles->pos.x && inside(obstacles->pos.y, bb.lowerBound.y, bb.upperBound.y)) obstacles->left = obstacles->left +1;
        if (bb.lowerBound.x > obstacles->pos.x && inside(obstacles->pos.y, bb.lowerBound.y, bb.upperBound.y)) obstacles->right = obstacles->right +1;
        if (bb.upperBound.y < obstacles->pos.y && inside(obstacles->pos.x, bb.lowerBound.x, bb.upperBound.x)) obstacles->bottom = obstacles->bottom +1;
        if (bb.lowerBound.y > obstacles->pos.y && inside(obstacles->pos.x, bb.lowerBound.x, bb.upperBound.x)) obstacles->top = obstacles->top +1;
    }
    return true; // continue with iteration
}

// change width : height ratio
// this is quite tricky since we also have to check that there
// is sufficient space in the scene
void transformRectPlayer(float step) {
    // check rotation to use proper axis for growing wider/taller
    float rot = b2Rot_GetAngle(b2Body_GetRotation(gRectPlayer));
    if (fabsf(rot)>(float)M_PI_4 && fabsf(fabsf(rot)-(float)M_PI)>(float)M_PI_4) {
        step = -step;
    }
    
    // change aspect ratio, clamping it to -8...+8 and skipping over -1..+1
    float oldRatio = gRectRatio; // in case we cannot grow
    gRectRatio += step;
    if (gRectRatio > 8.0f) gRectRatio=8.0f;
    if (gRectRatio < -8.0f) gRectRatio=-8.0f;
    if (gRectRatio > -1.0f && gRectRatio<1.0f) {
        gRectRatio = (step>=0.0f) ? 1.0f : -1.0f;
    }
    
    // construct updated shape
    float w, h;
    setRectWidthHeight(&w, &h);
    b2Polygon box = b2MakeBox(w,h);
    
    
    /* Depracted Code due b2World_OverlapPolygon
    // test whether the new shape fits into the world
    b2QueryFilter filter = b2DefaultQueryFilter();
    obstacles.pos = b2Body_GetPosition(gRectPlayer);
    obstacles.left = 0;
    obstacles.right = 0;
    obstacles.top = 0;
    obstacles.bottom = 0;
    
    b2World_OverlapPolygon(gWorldID, &box, b2Body_GetTransform(gRectPlayer), filter, (b2OverlapResultFcn*)rectTransformCheck, &obstacles);
    */


    b2QueryFilter filter = b2DefaultQueryFilter();
    obstacles.pos = b2Body_GetPosition(gRectPlayer);
    obstacles.left = 0;
    obstacles.right = 0;
    obstacles.top = 0;
    obstacles.bottom = 0;

    // calculate the AABB for the rotated box
    b2AABB aabb;
    float c = fabsf(cosf(rot));
    float s = fabsf(sinf(rot));
    float hx = c * w + s * h;
    float hy = s * w + c * h;
    
    aabb.lowerBound.x = obstacles.pos.x - hx;
    aabb.lowerBound.y = obstacles.pos.y - hy;
    aabb.upperBound.x = obstacles.pos.x + hx;
    aabb.upperBound.y = obstacles.pos.y + hy;

    b2World_OverlapAABB(
        gWorldID,
        aabb,
        filter,
        (b2OverlapResultFcn*)rectTransformCheck,
        &obstacles
    );


    //printf("pos: %f, %f;  left:%d right:%d top:%d bottom:%d\n", obstacles.pos.x, obstacles.pos.y, obstacles.left, obstacles.right, obstacles.top, obstacles.bottom);
    // only grow shape if not in contact from two opposing sides
    if (!(obstacles.right && obstacles.left) && !(obstacles.top && obstacles.bottom)) {
        b2Shape_SetPolygon(gRectShapeID, &box);
    } else {
        gRectRatio = oldRatio;
    }
}

// check if point is inside the rect player
// NB: point-in-polygon test in box2d requires transformation to local
// coordinates
int pointInRectPlayer(float x, float y) {
    b2Vec2 posGlobal;
    posGlobal.x = x;
    posGlobal.y = y;
    b2Vec2 posLocal = b2InvTransformPoint (b2Body_GetTransform(gRectPlayer), posGlobal);
    b2Polygon rect = b2Shape_GetPolygon(gRectShapeID);
    return b2PointInPolygon (posLocal, &rect);
}

//
// disc player movement
//

// returns 1 if body is in contact with another body below, 0 otherwise
// for the disc, normals of objects below point up
int discOnGround(void) {
    int cs = b2Body_GetContactData(gDiscPlayer, gContacts, MAX_CONTACTS);
    // search for contact 'below', i.e., with upward pointing normal
    for (int i=0; i<cs; i++) {
        for (int j=0; j<gContacts[i].manifold.pointCount; j++) {
            if (gContacts[i].manifold.normal.y>0.7) {
                // contact found
                return 1;
            }
        }
    }
    return 0;
}

void moveDiscPlayer(float f) {
    // only move if on a ground surface and not too fast
    if (discOnGround()) {
        b2Vec2 tmp = b2Body_GetLinearVelocity(gDiscPlayer);
        if (tmp.x*f/fabs(f) < 4.0f) {
            tmp.x = f;
            tmp.y = 0.0f;
            b2Body_ApplyLinearImpulseToCenter(gDiscPlayer, tmp, 1);
        }
    }
}

void jumpDiscPlayer(float f) {
    // only jump off a surface below
    if (discOnGround()) {
        b2Vec2 force;
        force.x = 0.0f;
        force.y = f;
        b2Body_ApplyLinearImpulseToCenter(gDiscPlayer, force, 1);
    }
}

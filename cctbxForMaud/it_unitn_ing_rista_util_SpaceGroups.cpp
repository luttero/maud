//
//  it_unitn_ing_rista_util_SpaceGroups.cpp
//  cctbxForMaud
//
//  Created by Luca Lutterotti on 09/05/15.
//  Copyright (c) 2015 Luca Lutterotti. All rights reserved.
//

#include "it_unitn_ing_rista_util_SpaceGroups.h"
#include "corediffraction/CrystalUtilities.h"

using namespace std;
using namespace radiographema::corediffraction;

/* extern "C"  specify the C calling convention */

JNIEXPORT jint JNICALL Java_it_unitn_ing_rista_util_SpaceGroups_testCCTBXForMaud
(JNIEnv *env, jclass) {
  return 1;
}

JNIEXPORT jobject JNICALL Java_it_unitn_ing_rista_util_SpaceGroups_getAllSymmetriesAndSpaceGroups
(JNIEnv *env, jclass obj) {
    
  jclass spacegroup = env->FindClass("it/unitn/ing/rista/util/Spacegroup");
  if (spacegroup != 0) {
    jmethodID java_sg_init = env->GetMethodID(spacegroup, "<init>", "(ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Z)V");
    if (java_sg_init != 0) {
      jmethodID java_sg_add = env->GetMethodID(spacegroup, "addSymops", "(Ljava/lang/String;)V");
      
      jclass crystalSystem = env->FindClass("it/unitn/ing/rista/util/CrystalSystem");
      jmethodID java_system_init = env->GetMethodID(crystalSystem, "<init>", "(Ljava/lang/String;)V");
      jmethodID java_system_add = env->GetMethodID(crystalSystem, "addSpaceGroupsObject", "(Lit/unitn/ing/rista/util/Spacegroup;)V");
      
      vector<CrystalSystem> allSystems = CrystalUtilities::getAllSymmetriesAndSpaceGroups();
      
      jclass systemVector = env->FindClass("java/util/Vector");
      jmethodID java_vector_init = env->GetMethodID(systemVector, "<init>", "(I)V");
      jmethodID java_vector_add = env->GetMethodID(systemVector, "addElement", "(Ljava/lang/Object;)V");
      jobject vectorToReturn =  env->NewObject(systemVector, java_vector_init, allSystems.size());
      for (int i = 0; i < allSystems.size(); i++) {
        CrystalSystem c_system = allSystems[i];
        jstring symmetry = env->NewStringUTF(c_system.symmetry.c_str());
        jobject system =  env->NewObject(crystalSystem, java_system_init, symmetry);
        for (int j = 0; j < c_system.space_groups.size(); j++) {
          Spacegroup sg = c_system.space_groups[j];
          cctbx::sgtbx::space_group s_g = CrystalUtilities::parseSpaceGroupString(sg.hall);
          jstring sf = env->NewStringUTF(sg.schoenflies.c_str());
          jstring hm = env->NewStringUTF(sg.hermann_mauguin.c_str());
          jstring h = env->NewStringUTF(sg.hall.c_str());
          jstring ct = env->NewStringUTF(sg.centering_type.c_str());
          jboolean jcent = (jboolean) s_g.is_centric();
          jobject jsg =  env->NewObject(spacegroup, java_sg_init, (jint) sg.number, sf, hm, h, ct, symmetry, jcent);
          for (int k = 0; k < sg.symops.size(); k++) {
            jstring sym = env->NewStringUTF(sg.symops[k].getSymopAsString().c_str());
            env->CallVoidMethod(jsg, java_sg_add, sym);
          }
          env->CallVoidMethod(system, java_system_add, jsg);
        }
        env->CallVoidMethod(vectorToReturn, java_vector_add, system);
      }
      
      return vectorToReturn;
    }
  }
  return NULL;
}

JNIEXPORT jobject JNICALL Java_it_unitn_ing_rista_util_SpaceGroups_checkSpaceGroup
(JNIEnv *env, jclass obj, jstring hall, jstring hm, jstring number) {
    jclass spacegroup = env->FindClass("it/unitn/ing/rista/util/Spacegroup");
    jmethodID java_sg_init = env->GetMethodID(spacegroup, "<init>", "(ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Z)V");
    jmethodID java_sg_add = env->GetMethodID(spacegroup, "addSymops", "(Ljava/lang/String;)V");
  
    const char *cstrHall = env->GetStringUTFChars(hall, 0);
    string hallStr(cstrHall);
    const char *cstrHM = env->GetStringUTFChars(hm, 0);
    string hmStr(cstrHM);
    const char *cstrNumber = env->GetStringUTFChars(number, 0);
    string numberStr(cstrNumber);
    
    Spacegroup sg = CrystalUtilities::checkSpaceGroup(hallStr, hmStr, numberStr);
    cctbx::sgtbx::space_group s_g = CrystalUtilities::parseSpaceGroupString(sg.hall);
    env->ReleaseStringUTFChars(hall, cstrHall);
    env->ReleaseStringUTFChars(hm, cstrHM);
    env->ReleaseStringUTFChars(number, cstrNumber);
    
    jstring jsf = env->NewStringUTF(sg.schoenflies.c_str());
    jstring jhm = env->NewStringUTF(sg.hermann_mauguin.c_str());
    jstring jh = env->NewStringUTF(sg.hall.c_str());
    jstring jct = env->NewStringUTF(sg.centering_type.c_str());
    jstring jsym = env->NewStringUTF(sg.symmetry.c_str());
    jboolean jcent = (jboolean) s_g.is_centric();
    jobject jsg =  env->NewObject(spacegroup, java_sg_init, (jint) sg.number, jsf, jhm, jh, jct, jsym, jcent);
    for (int k = 0; k < sg.symops.size(); k++) {
        jstring sym = env->NewStringUTF(sg.symops[k].getSymopAsString().c_str());
        env->CallVoidMethod(jsg, java_sg_add, sym);
    }
    return jsg;
}

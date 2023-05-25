package com.polus.fibicomp.coi.service;

import com.polus.fibicomp.coi.dao.GeneralDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.Map;

@Service
@Transactional
public class GeneralServiceImpl implements GeneralService{

    @Autowired
    private GeneralDao generalDao;

    @Override
    public ResponseEntity<Object> fetchAllCoiRights() {

        Map<String, Object> objectMap = new HashMap<>();
        objectMap.put("rights", generalDao.fetchAllCoiRights(AuthenticatedUser.getLoginPersonId()));
        objectMap.put("IS_REVIEW_MEMBER", generalDao.isPersonInReviewer(AuthenticatedUser.getLoginPersonId()));
        return new ResponseEntity<>(objectMap, HttpStatus.OK);
    }


}

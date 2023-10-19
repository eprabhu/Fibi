package com.polus.fibicomp.coi.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.coi.dao.GeneralDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.security.AuthenticatedUser;

@Service
@Transactional
public class GeneralServiceImpl implements GeneralService{

    @Autowired
    private GeneralDao generalDao;

    @Autowired
    private PersonDao personDao;

    @Autowired
    private CommonDao commonDao;

    @Override
	public ResponseEntity<Object> fetchAdminGroupsAndPersons() {
		List<String> personIds = personDao.getAdministratorsByModuleCode(Constants.COI_MODULE_CODE);
		Set<Person> persons = new HashSet<>();
		personIds.forEach(personId -> persons.add(personDao.getPersonDetailById(personId)));
		List<AdminGroup> adminGroups = commonDao.fetchAdminGroupsBasedOnModuleCode(Constants.COI_MODULE_CODE);
		Map<String, Object> objectMap = new HashMap<>();
		objectMap.put("persons", persons);
		objectMap.put("adminGroups", adminGroups);
		return new ResponseEntity<>(objectMap, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> fetchAllCoiOpaRights() {
		Map<String, Object> objectMap = new HashMap<>();
        List<String> rights = new ArrayList<>();
        rights.addAll(generalDao.fetchAllCoiOpaRights(AuthenticatedUser.getLoginPersonId()));
        objectMap.put("rights", rights);
        objectMap.put("IS_REVIEW_MEMBER", generalDao.isPersonInReviewer(AuthenticatedUser.getLoginPersonId()));
        return new ResponseEntity<>(objectMap, HttpStatus.OK);
	}

}

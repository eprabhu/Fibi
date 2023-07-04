package com.polus.fibicomp.coi.service;

import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.coi.dao.GeneralDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

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
    public ResponseEntity<Object> fetchAllCoiRights() {

        Map<String, Object> objectMap = new HashMap<>();
        objectMap.put("rights", generalDao.fetchAllCoiRights(AuthenticatedUser.getLoginPersonId()));
        objectMap.put("IS_REVIEW_MEMBER", generalDao.isPersonInReviewer(AuthenticatedUser.getLoginPersonId()));
        return new ResponseEntity<>(objectMap, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> fetchAdminGroupsAndPersons() {
        List<PersonRoles> personRoles = personDao.getPersonRolesByRoleAndRightId(Constants.COI_ADMINISTRATOR, Constants.VIEW_ADMIN_GROUP_COI);
        Set<Person> persons = new HashSet<>();
        personRoles.forEach(personRole -> {
            if (personRole.getPerson() != null) {
                persons.add(personRole.getPerson());
            }
        });

        List<AdminGroup> adminGroups = commonDao.fetchAdminGroupsBasedOnModuleCode(Constants.COI_MODULE_CODE);
        Map<String, Object> objectMap = new HashMap<>();
        objectMap.put("persons", persons);
        objectMap.put("adminGroups", adminGroups);

        return new ResponseEntity<>(objectMap, HttpStatus.OK);
    }
}

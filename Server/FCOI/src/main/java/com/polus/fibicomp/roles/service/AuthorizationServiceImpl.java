package com.polus.fibicomp.roles.service;

import java.util.ArrayList;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.roles.dao.AuthorizationServiceDao;

@Transactional
@Service(value = "authorizationService")
public class AuthorizationServiceImpl implements AuthorizationService {

	protected static Logger logger = LogManager.getLogger(AuthorizationServiceImpl.class.getName());

	@Autowired
	private AuthorizationServiceDao authorizationServiceDao;

	@Autowired
	private CommonDao commonDao;

	@Override
	public String allSystemLevelPermission(String personId) {
		return commonDao.convertObjectToJSON(authorizationServiceDao.allSystemLevelPermission(personId));
	}

	@Override
	public boolean isPersonHasRightInDepartment(String rightName, String personId, String unitNumber) {
		return authorizationServiceDao.isPersonHasRightInDepartment(rightName, personId, unitNumber);
	}

	@Override
	public boolean isPersonHasRightInaDocument(String rightName, String personId, Integer moduleCode, String moduleItemKey) {
		return authorizationServiceDao.isPersonHasRightInaDocument(rightName, personId, moduleCode, moduleItemKey);
	}

	@Override
	public boolean isPersonHasRightInAnyDepartment(String rightName, String personId) {
		return authorizationServiceDao.isPersonHasRightInAnyDepartment(rightName, personId);
	}

	@Override
	public ArrayList<String> allDepartmentPermission(Integer moduleCode, String personId, String leadUnit, Integer moduleItemKey) {
		return authorizationServiceDao.allDepartmentLevelPermission(moduleCode, personId, leadUnit, moduleItemKey);
	}

}

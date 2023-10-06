package com.polus.fibicomp.opa.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.opa.dao.OPADao;

@Transactional
@Service(value = "opaService")
public class OPAServiceImpl implements OPAService {

	@Autowired
	private OPADao opaDao;

	@Override
	public Boolean canCreateOpaDisclosure(String personId) {
		return opaDao.isOpaDisclosureRequired(personId);
	}

	@Override
	public ResponseEntity<Object> createOpaDisclosure(String personId, String homeUnit) {
		Integer opaDisclosureId = opaDao.createOpaDisclosure(personId, homeUnit);
		return new ResponseEntity<>(opaDisclosureId, HttpStatus.OK);
	}

}

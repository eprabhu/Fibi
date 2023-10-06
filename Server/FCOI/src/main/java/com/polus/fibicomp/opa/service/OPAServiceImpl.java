package com.polus.fibicomp.opa.service;

import com.polus.fibicomp.coi.service.ActionLogService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPACommonDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.opa.dao.OPADao;

import java.sql.Timestamp;

@Transactional
@Service(value = "opaService")
public class OPAServiceImpl implements OPAService {

	@Autowired
	private OPADao opaDao;

	@Autowired
	private ActionLogService actionLogService;

	@Autowired
	private PersonDao personDao;

	@Override
	public Boolean canCreateOpaDisclosure(String personId) {
		return opaDao.isOpaDisclosureRequired(personId);
	}

	@Override
	public ResponseEntity<Object> createOpaDisclosure(String personId, String homeUnit) {
		opaDao.createOpaDisclosure(personId, homeUnit);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_CREATED, opaCommonDto);
		return new ResponseEntity<>(personId, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> submitOPADisclosure(OPASubmitDto opaSubmitDto) {
		if(opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_SUBMIT,
				Constants.OPA_DISPOSITION_STATUS_PENDING, opaSubmitDto.getOpaDisclosureId())) {
			return new ResponseEntity<>("Already Submitted", HttpStatus.METHOD_NOT_ALLOWED);
		}
		Timestamp timestamp = opaDao.submitOPADisclosure(opaSubmitDto);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.opaDisclosureId(opaSubmitDto.getOpaDisclosureId())
				.opaDisclosureNumber(opaSubmitDto.getOpaDisclosureNumber())
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_SUBMITTED, opaCommonDto);
		return new ResponseEntity<>(timestamp, HttpStatus.OK);
	}


	@Override
	public ResponseEntity<Object> withdrawOPADisclosure(Integer opaDisclosureId, String opaDisclosureNumber) {
		if(opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_WITHDRAW,
				null, opaDisclosureId)) {
			return new ResponseEntity<>("Already withdrawn", HttpStatus.METHOD_NOT_ALLOWED);
		}
		Timestamp timestamp = opaDao.returnOrWithdrawOPADisclosure(Constants.OPA_DISCLOSURE_STATUS_WITHDRAW,
				opaDisclosureId);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.opaDisclosureId(opaDisclosureId)
				.opaDisclosureNumber(opaDisclosureNumber)
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_WITHDRAWN, opaCommonDto);
		return new ResponseEntity<>(timestamp, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> returnOPADisclosure(Integer opaDisclosureId, String opaDisclosureNumber) {
		if(opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_RETURN,
				null, opaDisclosureId)) {
			return new ResponseEntity<>("Already returned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		Timestamp timestamp = opaDao.returnOrWithdrawOPADisclosure(Constants.OPA_DISCLOSURE_STATUS_RETURN,
				opaDisclosureId);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.opaDisclosureId(opaDisclosureId)
				.opaDisclosureNumber(opaDisclosureNumber)
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_RETURNED, opaCommonDto);
		return new ResponseEntity<>(timestamp, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> assignAdminOPADisclosure(OPAAssignAdminDto assignAdminDto) {
		if(opaDao.isAdminAssigned( assignAdminDto.getOpaDisclosureId())) {
			return new ResponseEntity<>("Already admin assigned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		Timestamp timestamp = opaDao.assignAdminOPADisclosure(assignAdminDto);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.opaDisclosureId(assignAdminDto.getOpaDisclosureId())
				.opaDisclosureNumber(assignAdminDto.getOpaDisclosureNumber())
				.adminPersonName(personDao.getPersonFullNameByPersonId(assignAdminDto.getAdminPersonId()))
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_ADMIN_ASSIGNED, opaCommonDto);
		return new ResponseEntity<>(timestamp, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> completeOPADisclosure(Integer opaDisclosureId, String opaDisclosureNumber) {
		if(opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_COMPLETED,
				Constants.OPA_DISPOSITION_STATUS_COMPLETED, opaDisclosureId)) {
			return new ResponseEntity<>("Already approved", HttpStatus.METHOD_NOT_ALLOWED);
		}
		Timestamp timestamp = opaDao.completeOPADisclosure(opaDisclosureId);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.opaDisclosureId(opaDisclosureId)
				.opaDisclosureNumber(opaDisclosureNumber)
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_APPROVED, opaCommonDto);
		return new ResponseEntity<>(timestamp, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> reassignAdminOPADisclosure(OPAAssignAdminDto assignAdminDto) {
		Timestamp timestamp = opaDao.assignAdminOPADisclosure(assignAdminDto);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.adminPersonName(personDao.getPersonFullNameByPersonId(assignAdminDto.getAdminPersonId()))
				.opaDisclosureId(assignAdminDto.getOpaDisclosureId())
				.opaDisclosureNumber(assignAdminDto.getOpaDisclosureNumber())
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_ADMIN_REASSIGNED, opaCommonDto);
		return new ResponseEntity<>(timestamp, HttpStatus.OK);
	}
}

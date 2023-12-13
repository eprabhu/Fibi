package com.polus.fibicomp.opa.service;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.service.ActionLogService;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.opa.clients.FormBuilderClient;
import com.polus.fibicomp.opa.clients.model.ApplicableFormRequest;
import com.polus.fibicomp.opa.clients.model.ApplicableFormResponse;
import com.polus.fibicomp.opa.dao.OPADao;
import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPACommonDto;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import com.polus.fibicomp.opa.pojo.OPADisclosure;
import com.polus.fibicomp.opa.pojo.OPAFormBuilderDetails;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "opaService")
public class OPAServiceImpl implements OPAService {

	@Autowired
	private OPADao opaDao;

	@Autowired
	private ActionLogService actionLogService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private FormBuilderClient formBuilderClient;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private ConflictOfInterestDao conflictOfInterestDao;

	@Override
	public Boolean canCreateOpaDisclosure(String personId) {
		return opaDao.canCreateOpaDisclosure(personId);
	}

	protected static Logger logger = LogManager.getLogger(OPAServiceImpl.class.getName());
	
	@Override
	public ResponseEntity<Object> createOpaDisclosure(String personId, String homeUnit) {
		OPACommonDto opaDisclosure = opaDao.createOpaDisclosure(personId, homeUnit);
		OPACommonDto opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_CREATED, opaCommonDto);
		// TODO if needed move the below client call to any util class
		ApplicableFormRequest requestObject = ApplicableFormRequest.builder()
				.moduleItemCode(Constants.OPA_MODULE_ITEM_CODE)
				.moduleSubItemCode(Constants.OPA_MODULE_SUB_ITEM_CODE)
				.build();
		ResponseEntity<ApplicableFormResponse> response = formBuilderClient.getApplicableForms(requestObject);
		ApplicableFormResponse formResponse = response.getBody();
		OPAFormBuilderDetails opaFormBuilderDetails = OPAFormBuilderDetails.builder()
				.opaDisclosureId(opaDisclosure.getOpaDisclosureId())
				.opaDisclosureNumber(opaDisclosure.getOpaDisclosureNumber())
				.personId(AuthenticatedUser.getLoginPersonId())
				.formBuilderId(formResponse != null ? formResponse.getFormsBuilderId() : null)
				.isPrimaryForm(true)
				.updateTimestamp(commonDao.getCurrentTimestamp())
				.updateUser(AuthenticatedUser.getLoginUserName())
				.build();
		opaDao.saveOrUpdateOpaFormBuilderDetails(opaFormBuilderDetails);
		formResponse.setOpaDisclosureId(opaDisclosure.getOpaDisclosureId());
		return new ResponseEntity<>(formResponse, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> submitOPADisclosure(OPASubmitDto opaSubmitDto) {
		if(opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_SUBMIT,
				Constants.OPA_DISPOSITION_STATUS_PENDING, opaSubmitDto.getOpaDisclosureId())) {
			return new ResponseEntity<>("Already Submitted", HttpStatus.METHOD_NOT_ALLOWED);
		}
		if(opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_RETURN,
				Constants.OPA_DISPOSITION_STATUS_PENDING, opaSubmitDto.getOpaDisclosureId())) {
			opaSubmitDto.setOpaDisclosureStatus(Constants.OPA_DISCLOSURE_STATUS_REVIEW_IN_PROGRESS);
		}
		opaDao.submitOPADisclosure(opaSubmitDto);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.opaDisclosureId(opaSubmitDto.getOpaDisclosureId())
				.opaDisclosureNumber(opaSubmitDto.getOpaDisclosureNumber())
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_SUBMITTED, opaCommonDto);
		return getOPADisclosure(opaSubmitDto.getOpaDisclosureId());
	}


	@Override
	public ResponseEntity<Object> withdrawOPADisclosure(OPACommonDto opaCommonDto) {
		if (opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_WITHDRAW, null, opaCommonDto.getOpaDisclosureId())) {
			return new ResponseEntity<>("Already withdrawn", HttpStatus.METHOD_NOT_ALLOWED);
		}
		opaDao.returnOrWithdrawOPADisclosure(Constants.OPA_DISCLOSURE_STATUS_WITHDRAW, opaCommonDto.getOpaDisclosureId());
		OPACommonDto  dto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.comment(opaCommonDto.getComment())
				.opaDisclosureId(opaCommonDto.getOpaDisclosureId())
				.opaDisclosureNumber(opaCommonDto.getOpaDisclosureNumber())
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_WITHDRAWN, dto);
		return getOPADisclosure(opaCommonDto.getOpaDisclosureId());
	}

	@Override
	public ResponseEntity<Object> returnOPADisclosure(OPACommonDto opaCommonDto) {
		if (opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_RETURN, null, opaCommonDto.getOpaDisclosureId())) {
			return new ResponseEntity<>("Already returned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		opaDao.returnOrWithdrawOPADisclosure(Constants.OPA_DISCLOSURE_STATUS_RETURN, opaCommonDto.getOpaDisclosureId());
		OPACommonDto  dto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.comment(opaCommonDto.getComment())
				.opaDisclosureId(opaCommonDto.getOpaDisclosureId())
				.opaDisclosureNumber(opaCommonDto.getOpaDisclosureNumber())
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_RETURNED, dto);
		return getOPADisclosure(opaCommonDto.getOpaDisclosureId());
	}

	@Override
	public ResponseEntity<Object> assignAdminOPADisclosure(OPAAssignAdminDto assignAdminDto) {
		try {
			saveAssignAdminActionLog(assignAdminDto.getAdminPersonId(), assignAdminDto.getOpaDisclosureId(), assignAdminDto.getOpaDisclosureNumber());
		} catch (Exception e) {
			logger.error("assignDisclosureAdmin : {}", e.getMessage());
		}
		assignAdminDto.setOpaDisclosureStatus(Boolean.TRUE.equals(opaDao.isAdminAssigned(assignAdminDto.getOpaDisclosureId())) 
						? null
						: Constants.OPA_DISCLOSURE_STATUS_REVIEW_IN_PROGRESS);
		opaDao.assignAdminOPADisclosure(assignAdminDto);
		return getOPADisclosure(assignAdminDto.getOpaDisclosureId());
	}

	private Boolean saveAssignAdminActionLog(String adminPersonId, Integer opaDisclosureId, String opaDisclosureNumber) {
		Boolean isAdminAssigned = opaDao.isAdminAssigned(opaDisclosureId);
		if (Boolean.TRUE.equals(isAdminAssigned)) {
			OPACommonDto opaCommonDto = OPACommonDto.builder()
					.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
					.adminPersonName(personDao.getPersonFullNameByPersonId(opaDao.getAssignedAdmin(opaDisclosureId)))
					.reassignedAdminPersonName(personDao.getPersonFullNameByPersonId(adminPersonId))
					.opaDisclosureId(opaDisclosureId).opaDisclosureNumber(opaDisclosureNumber).build();
			actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_ADMIN_REASSIGNED, opaCommonDto);
		} else {
			OPACommonDto opaCommonDto = OPACommonDto.builder()
					.updateUserFullName(AuthenticatedUser.getLoginUserFullName()).opaDisclosureId(opaDisclosureId)
					.opaDisclosureNumber(opaDisclosureNumber).adminPersonName(personDao.getPersonFullNameByPersonId(adminPersonId))
					.build();
			actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_ADMIN_ASSIGNED, opaCommonDto);
		}
		return isAdminAssigned;
	}

	@Override
	public ResponseEntity<Object> completeOPADisclosure(Integer opaDisclosureId, String opaDisclosureNumber) {
		if(opaDao.isOPAWithStatuses(Constants.OPA_DISCLOSURE_STATUS_COMPLETED, Constants.OPA_DISPOSITION_STATUS_COMPLETED, opaDisclosureId)) {
			return new ResponseEntity<>("Already approved", HttpStatus.METHOD_NOT_ALLOWED);
		}
		opaDao.completeOPADisclosure(opaDisclosureId);
		OPACommonDto  opaCommonDto = OPACommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.opaDisclosureId(opaDisclosureId)
				.opaDisclosureNumber(opaDisclosureNumber)
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_APPROVED, opaCommonDto);
		return getOPADisclosure(opaDisclosureId);
	}

	@Override
	public ResponseEntity<Object> getOPADisclosure(Integer opaDisclosureId) {
		OPADisclosure opaDisclosure = opaDao.getOPADisclosure(opaDisclosureId);
		opaDisclosure.setUpdateUserFullName(personDao.getUserFullNameByUserName(opaDisclosure.getUpdateUser()));
		opaDisclosure.setAdminGroupName(opaDisclosure.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(opaDisclosure.getAdminGroupId()).getAdminGroupName() : null);
		opaDisclosure.setAdminPersonName(opaDisclosure.getAdminPersonId() != null ? personDao.getPersonFullNameByPersonId(opaDisclosure.getAdminPersonId()) : null);
		Person person = personDao.getPersonDetailById(opaDisclosure.getPersonId());
		opaDisclosure.setPersonEmail(person.getEmailAddress());
		opaDisclosure.setPersonPrimaryTitle(person.getPrimaryTitle());
		opaDisclosure.setHomeUnitName(commonDao.getUnitName(opaDisclosure.getHomeUnit()));
		opaDisclosure.setOpaFormBuilderDetails(opaDao.getOpaFormBuilderDetailsByOpaDisclosureId(opaDisclosureId));
		opaDisclosure.setPersonAttachmentsCount(conflictOfInterestDao.personAttachmentsCount(opaDisclosure.getPersonId()));
		opaDisclosure.setPersonNotesCount(conflictOfInterestDao.personNotesCount(opaDisclosure.getPersonId()));
		opaDisclosure.setPersonEntitiesCount(conflictOfInterestDao.getSFIOfDisclosureCount(ConflictOfInterestVO.builder().personId(opaDisclosure.getPersonId()).build()));
		return new ResponseEntity<>(opaDisclosure, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getOPADashboard(OPADashboardRequestDto requestDto) {
		return new ResponseEntity<>(opaDao.getOPADashboard(requestDto), HttpStatus.OK);
	}
}

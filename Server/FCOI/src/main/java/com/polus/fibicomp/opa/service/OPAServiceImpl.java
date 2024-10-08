package com.polus.fibicomp.opa.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.constants.CoreConstants;
import com.polus.core.person.dao.PersonDao;
import com.polus.core.person.pojo.Person;
import com.polus.fibicomp.config.CustomExceptionService;
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
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.coi.clients.FormBuilderClient;
import com.polus.fibicomp.coi.clients.model.ApplicableFormRequest;
import com.polus.fibicomp.coi.clients.model.ApplicableFormResponse;
import com.polus.fibicomp.opa.dao.OPADao;
import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPACommonDto;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import com.polus.fibicomp.opa.pojo.OPADisclosure;
import com.polus.fibicomp.opa.pojo.OPAFormBuilderDetails;
import com.polus.core.security.AuthenticatedUser;

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

	@Autowired
	private CustomExceptionService exceptionService;

	@Override
	public Boolean canCreateOpaDisclosure(String personId) {
		return opaDao.canCreateOpaDisclosure(personId);
	}

	protected static Logger logger = LogManager.getLogger(OPAServiceImpl.class.getName());
	
	@Override
	public ResponseEntity<Object> createOpaDisclosure(String personId, String homeUnit) {
		OPACommonDto opaDisclosure = opaDao.createOpaDisclosure(personId, homeUnit);
		OPACommonDto opaCommonDto = OPACommonDto.builder()
				.opaDisclosureId(opaDisclosure.getOpaDisclosureId())
				.opaDisclosureNumber(opaDisclosure.getOpaDisclosureNumber())
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.build();
		actionLogService.saveOPAActionLog(Constants.OPA_ACTION_LOG_TYPE_CREATED, opaCommonDto);
		// TODO if needed move the below client call to any util class
		ApplicableFormRequest requestObject = ApplicableFormRequest.builder()
				.moduleItemCode(Constants.OPA_MODULE_ITEM_CODE)
				.moduleSubItemCode(Constants.OPA_MODULE_SUB_ITEM_CODE)
				.documentOwnerPersonId(AuthenticatedUser.getLoginPersonId())
				.build();
		ResponseEntity<ApplicableFormResponse> response = formBuilderClient.getApplicableForms(requestObject);
		ApplicableFormResponse formResponse = response.getBody();
		List<Integer> formBuilderIds = formResponse != null ?formResponse.getApplicableFormsBuilderIds(): new ArrayList<>();
		if (formBuilderIds != null && !formBuilderIds.isEmpty()) {
			boolean isPrimaryForm = true;
			for (Integer formBuilderId : formBuilderIds) {
				OPAFormBuilderDetails opaFormBuilderDetails = OPAFormBuilderDetails.builder()
						.opaDisclosureId(opaDisclosure.getOpaDisclosureId())
						.opaDisclosureNumber(opaDisclosure.getOpaDisclosureNumber())
						.personId(AuthenticatedUser.getLoginPersonId()).formBuilderId(formBuilderId)
						.isPrimaryForm(isPrimaryForm).updateTimestamp(commonDao.getCurrentTimestamp())
						.updateUser(AuthenticatedUser.getLoginUserName()).build();
				opaDao.saveOrUpdateOpaFormBuilderDetails(opaFormBuilderDetails);
				isPrimaryForm = false;
			}
		}
		if (formResponse != null) {
		    formResponse.setDisclosureId(opaDisclosure.getOpaDisclosureId());
		}		
		return new ResponseEntity<>(formResponse, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> submitOPADisclosure(OPASubmitDto opaSubmitDto) {
		List<String> opaDisclosureStatuses = new ArrayList<>(Arrays.asList(Constants.OPA_DISCLOSURE_STATUS_SUBMIT,
			    Constants.OPA_DISCLOSURE_STATUS_REVIEW_IN_PROGRESS, Constants.OPA_DISCLOSURE_STATUS_REVIEW_ASSIGNED,
			    Constants.OPA_DISCLOSURE_STATUS_REVIEW_COMPLETED));
		if(opaDao.isOPAWithStatuses(opaDisclosureStatuses, Constants.OPA_DISPOSITION_STATUS_PENDING, opaSubmitDto.getOpaDisclosureId())) {
			return new ResponseEntity<>("Already Submitted", HttpStatus.METHOD_NOT_ALLOWED);
		}
		opaDisclosureStatuses.clear();
		opaDisclosureStatuses.add(Constants.OPA_DISCLOSURE_STATUS_RETURN);
		if (opaDao.isOPAWithStatuses(opaDisclosureStatuses, Constants.OPA_DISPOSITION_STATUS_PENDING, opaSubmitDto.getOpaDisclosureId())) {
			if (Boolean.TRUE.equals(conflictOfInterestDao.isOpaReviewerAssigned(opaSubmitDto.getOpaDisclosureId()))) {
				if (Boolean.TRUE.equals(conflictOfInterestDao.isOpaReviewerReviewCompleted(opaSubmitDto.getOpaDisclosureId()))) {
					opaSubmitDto.setOpaDisclosureStatus(Constants.OPA_DISCLOSURE_STATUS_REVIEW_COMPLETED);
				} else {
					opaSubmitDto.setOpaDisclosureStatus(Constants.OPA_DISCLOSURE_STATUS_REVIEW_ASSIGNED);
				}
			} else {
				opaSubmitDto.setOpaDisclosureStatus(Constants.OPA_DISCLOSURE_STATUS_REVIEW_IN_PROGRESS);
			}
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
		List<String> opaDisclosureStatus = Arrays.asList(Constants.OPA_DISCLOSURE_STATUS_SUBMIT);
		if (!opaDao.isOPAWithStatuses(opaDisclosureStatus, null, opaCommonDto.getOpaDisclosureId())) {
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
		List<String> opaDisclosureStatus = Arrays.asList(Constants.OPA_DISCLOSURE_STATUS_RETURN);
		if (opaDao.isOPAWithStatuses(opaDisclosureStatus, null, opaCommonDto.getOpaDisclosureId())) {
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
		if ((assignAdminDto.getActionType().equals("R") && opaDao.isSameAdminPersonOrGroupAdded(assignAdminDto.getAdminGroupId(), assignAdminDto.getAdminPersonId(), assignAdminDto.getOpaDisclosureId()))
				|| (assignAdminDto.getActionType().equals("A") && opaDao.isAdminPersonOrGroupAdded(assignAdminDto.getOpaDisclosureId()))) {
			return new ResponseEntity<>("Admin already assigned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		if (assignAdminDto.getActionType().equals("A")) {
			List<String> opaDisclosureStatus = Arrays.asList(Constants.OPA_DISCLOSURE_STATUS_SUBMIT);
			if (!opaDao.isOPAWithStatuses(opaDisclosureStatus, null, assignAdminDto.getOpaDisclosureId())) {
				return new ResponseEntity<>("Assign admin action cannot be performed", HttpStatus.METHOD_NOT_ALLOWED);
			}
		}
		if (assignAdminDto.getActionType().equals("R")) {
			List<String> opaDisclosureStatus = Arrays.asList(Constants.OPA_DISCLOSURE_STATUS_RETURN, Constants.OPA_DISCLOSURE_STATUS_COMPLETED);
			if (opaDao.isOPAWithStatuses(opaDisclosureStatus, null, assignAdminDto.getOpaDisclosureId())) {
				return new ResponseEntity<>("Reassign admin action cannot be performed", HttpStatus.METHOD_NOT_ALLOWED);
			}
		}
		try {
			saveAssignAdminActionLog(assignAdminDto.getAdminPersonId(), assignAdminDto.getOpaDisclosureId(), assignAdminDto.getOpaDisclosureNumber());
		} catch (Exception e) {
			logger.error("assignDisclosureAdmin : {}", e.getMessage());
			exceptionService.saveErrorDetails(e.getMessage(), e, CoreConstants.JAVA_ERROR);
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
		List<String> opaDisclosureStatus = Arrays.asList(Constants.OPA_DISCLOSURE_STATUS_COMPLETED);
		if(opaDao.isOPAWithStatuses(opaDisclosureStatus, Constants.OPA_DISPOSITION_STATUS_COMPLETED, opaDisclosureId)) {
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
		List<OPAFormBuilderDetails> opaFormBuilderDetail = opaDao.getOpaFormBuilderDetailsByOpaDisclosureId(opaDisclosureId);
		opaDisclosure.setOpaFormBuilderDetails(opaFormBuilderDetail);
		opaDisclosure.setPersonAttachmentsCount(conflictOfInterestDao.personAttachmentsCount(opaDisclosure.getPersonId()));
		opaDisclosure.setPersonNotesCount(conflictOfInterestDao.personNotesCount(opaDisclosure.getPersonId()));
		opaDisclosure.setPersonEntitiesCount(conflictOfInterestDao.getSFIOfDisclosureCount(ConflictOfInterestVO.builder().personId(opaDisclosure.getPersonId()).build()));

		/**
		 * Intentionally commented. Logic to check for new form when the document is in edit mode

		List<String> editModeOPADisclosureStatusCodes = Arrays.asList(Constants.OPA_DISCLOSURE_STATUS_PENDING, Constants.OPA_DISCLOSURE_STATUS_RETURN, Constants.OPA_DISCLOSURE_STATUS_WITHDRAW);
		if (editModeOPADisclosureStatusCodes.contains(opaDisclosure.getReviewStatusCode())) {
			ApplicableFormRequest requestObject = ApplicableFormRequest.builder()
					.moduleItemCode(Constants.OPA_MODULE_ITEM_CODE)
					.moduleSubItemCode(Constants.OPA_MODULE_SUB_ITEM_CODE)
					.documentOwnerPersonId(AuthenticatedUser.getLoginPersonId())
					.build();
			ResponseEntity<ApplicableFormResponse> response = formBuilderClient.getApplicableForms(requestObject);
			ApplicableFormResponse formResponse = response.getBody();
			List<Integer> formBuilderIds = formResponse != null ?formResponse.getApplicableFormsBuilderIds(): new ArrayList<>();
			List<OPAFormBuilderDetails> opaFormBuilderDetails = opaDao.getOpaFormBuilderDetailsByOpaDisclosureId(opaDisclosureId);
			formBuilderIds.stream()
	        .filter(formBuilderId -> opaFormBuilderDetails.stream()
	                .map(OPAFormBuilderDetails::getFormBuilderId)
	                .noneMatch(id -> id.equals(formBuilderId)))
	        .forEach(formBuilderId -> {
	            OPAFormBuilderDetails opaFormBuilderDetailEntity = OPAFormBuilderDetails.builder()
	                    .opaDisclosureId(opaDisclosure.getOpaDisclosureId())
	                    .opaDisclosureNumber(opaDisclosure.getOpaDisclosureNumber())
	                    .personId(AuthenticatedUser.getLoginPersonId())
	                    .formBuilderId(formBuilderId)
	                    .isPrimaryForm(Boolean.FALSE)
	                    .updateTimestamp(commonDao.getCurrentTimestamp())
	                    .updateUser(AuthenticatedUser.getLoginUserName())
	                    .build();
	            opaDao.saveOrUpdateOpaFormBuilderDetails(opaFormBuilderDetailEntity);
	            opaFormBuilderDetail.add(opaFormBuilderDetailEntity);
	        });
		}

		*/
		return new ResponseEntity<>(opaDisclosure, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getOPADashboard(OPADashboardRequestDto requestDto) {
		return new ResponseEntity<>(opaDao.getOPADashboard(requestDto), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getOpaPersonType() {
		return new ResponseEntity<>(opaDao.getOpaPersonType(), HttpStatus.OK);
	}

}

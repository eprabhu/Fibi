package com.polus.fibicomp.disclosures.consultingdisclosure.service;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.coi.clients.FormBuilderClient;
import com.polus.fibicomp.coi.clients.model.ApplicableFormRequest;
import com.polus.fibicomp.coi.clients.model.ApplicableFormResponse;
import com.polus.fibicomp.disclosures.consultingdisclosure.dao.ConsultingDisclosureDao;
import com.polus.fibicomp.disclosures.consultingdisclosure.dto.ConsultDisclAssignAdminDto;
import com.polus.fibicomp.disclosures.consultingdisclosure.dto.ConsultDisclCommonDto;
import com.polus.fibicomp.disclosures.consultingdisclosure.dto.ConsultDisclSubmitDto;
import com.polus.fibicomp.disclosures.consultingdisclosure.pojo.ConsultingDisclFormBuilderDetails;
import com.polus.fibicomp.disclosures.consultingdisclosure.pojo.ConsultingDisclosure;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service
public class ConsultingDisclosureServiceImpl implements ConsultingDisclosureService {

	protected static Logger logger = LogManager.getLogger(ConsultingDisclosureServiceImpl.class.getName());

	@Autowired
	private ConsultingDisclosureDao consultingDisclosureDao;

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
	public ResponseEntity<Object> createConsultingDisclosure(String personId, String homeUnit) {
		Timestamp timeStamp = commonDao.getCurrentTimestamp();
		ConsultingDisclosure consultingDisclosure = ConsultingDisclosure.builder()
				.personId(personId)
				.homeUnit(homeUnit)
				.createUser(AuthenticatedUser.getLoginUserName())
				.createTimestamp(timeStamp)
				.updateTimeStamp(timeStamp)
				.updateUser(AuthenticatedUser.getLoginUserName())
				.reviewStatusCode(Constants.CONSULT_DISCL_STATUS_PENDING)
				.dispositionStatusCode(Constants.CONSULT_DISCL_DISPOSITION_STATUS_PENDING)
				.build();
		ConsultingDisclosure disclosure = consultingDisclosureDao.createConsultingDisclosure(consultingDisclosure);
		ConsultDisclCommonDto consultDisclCommonDto = ConsultDisclCommonDto.builder()
				.disclosureId(disclosure.getDisclosureId())
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.build();
		actionLogService.saveConsultingDisclActionLog(Constants.CONSULT_DISCL_ACTION_LOG_TYPE_CREATED, consultDisclCommonDto);
		ApplicableFormRequest requestObject = ApplicableFormRequest.builder()
				.moduleItemCode(Constants.CONSULT_DISCL_MODULE_ITEM_CODE)
				.moduleSubItemCode(Constants.CONSULT_DISCL_MODULE_SUB_ITEM_CODE)
				.documentOwnerPersonId(AuthenticatedUser.getLoginPersonId())
				.build();
		ResponseEntity<ApplicableFormResponse> response = formBuilderClient.getApplicableForms(requestObject);
		ApplicableFormResponse formResponse = response.getBody();
		List<Integer> formBuilderIds = formResponse != null ? formResponse.getApplicableFormsBuilderIds() : new ArrayList<>();
		if (formBuilderIds != null && !formBuilderIds.isEmpty()) {
			boolean isPrimaryForm = true;
			for (Integer formBuilderId : formBuilderIds) {
				ConsultingDisclFormBuilderDetails consultingDisclFormBuilderDetails = ConsultingDisclFormBuilderDetails.builder()
						.disclosureId(disclosure.getDisclosureId())
						.personId(AuthenticatedUser.getLoginPersonId()).formBuilderId(formBuilderId)
						.isPrimaryForm(isPrimaryForm).updateTimestamp(timeStamp)
						.updateUser(AuthenticatedUser.getLoginUserName()).build();
				consultingDisclosureDao.saveOrUpdateConsultDisclFormBuilderDetails(consultingDisclFormBuilderDetails);
				isPrimaryForm = false;
			}
		}
		if (formResponse != null) {
		    formResponse.setDisclosureId(disclosure.getDisclosureId());
		}		
		return new ResponseEntity<>(formResponse, HttpStatus.OK);	
	}

	@Override
	public ResponseEntity<Object> submitConsultingDisclosure(ConsultDisclSubmitDto consultDisclSubmitDto) {
		List<String> consultingDisclStatuses = new ArrayList<>(Arrays.asList(Constants.CONSULT_DISCL_STATUS_SUBMIT, 
				Constants.CONSULT_DISCL_STATUS_REVIEW_IN_PROGRESS, Constants.CONSULT_DISCL_STATUS_COMPLETED));
		if (consultingDisclosureDao.isConsultingDisclWithStatuses(consultingDisclStatuses, Constants.CONSULT_DISCL_DISPOSITION_STATUS_PENDING, consultDisclSubmitDto.getDisclosureId())) {
			return new ResponseEntity<>("Already Submitted", HttpStatus.METHOD_NOT_ALLOWED);
		}
		consultingDisclStatuses.clear();
		consultingDisclStatuses.add(Constants.CONSULT_DISCL_STATUS_RETURN);
		if (consultingDisclosureDao.isConsultingDisclWithStatuses(consultingDisclStatuses, Constants.CONSULT_DISCL_DISPOSITION_STATUS_PENDING, consultDisclSubmitDto.getDisclosureId())) {
			consultDisclSubmitDto.setDisclosureStatus(Constants.CONSULT_DISCL_STATUS_REVIEW_IN_PROGRESS);
		}
		consultingDisclosureDao.submitConsultingDisclosure(consultDisclSubmitDto);
		ConsultDisclCommonDto consultDisclCommonDto = ConsultDisclCommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.disclosureId(consultDisclSubmitDto.getDisclosureId()).build();
		actionLogService.saveConsultingDisclActionLog(Constants.CONSULT_DISCL_ACTION_LOG_TYPE_SUBMITTED,
				consultDisclCommonDto);
		return getConsultingDisclosure(consultDisclSubmitDto.getDisclosureId());
	}

	@Override
	public ResponseEntity<Object> withdrawConsultingDisclosure(ConsultDisclCommonDto consultDisclCommonDto) {
		List<String> consultingDisclStatuses = Arrays.asList(Constants.CONSULT_DISCL_STATUS_SUBMIT);
		if (!consultingDisclosureDao.isConsultingDisclWithStatuses(consultingDisclStatuses, null, consultDisclCommonDto.getDisclosureId())) {
			return new ResponseEntity<>("Already withdrawn", HttpStatus.METHOD_NOT_ALLOWED);
		}
		consultingDisclosureDao.returnOrWithdrawConsultingDisclosure(Constants.CONSULT_DISCL_STATUS_WITHDRAW, consultDisclCommonDto.getDisclosureId());
		ConsultDisclCommonDto  dto = ConsultDisclCommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.comment(consultDisclCommonDto.getComment())
				.disclosureId(consultDisclCommonDto.getDisclosureId())
				.build();
		actionLogService.saveConsultingDisclActionLog(Constants.CONSULT_DISCL_ACTION_LOG_TYPE_WITHDRAWN, dto);
		return getConsultingDisclosure(consultDisclCommonDto.getDisclosureId());
	}

	@Override
	public ResponseEntity<Object> returnConsultingDisclosure(ConsultDisclCommonDto consultDisclCommonDto) {
		List<String> consultingDisclStatuses = Arrays.asList(Constants.CONSULT_DISCL_STATUS_RETURN);
		if (consultingDisclosureDao.isConsultingDisclWithStatuses(consultingDisclStatuses, null, consultDisclCommonDto.getDisclosureId())) {
			return new ResponseEntity<>("Already returned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		consultingDisclosureDao.returnOrWithdrawConsultingDisclosure(Constants.CONSULT_DISCL_STATUS_RETURN, consultDisclCommonDto.getDisclosureId());
		ConsultDisclCommonDto  dto = ConsultDisclCommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.comment(consultDisclCommonDto.getComment())
				.disclosureId(consultDisclCommonDto.getDisclosureId())
				.build();
		actionLogService.saveConsultingDisclActionLog(Constants.CONSULT_DISCL_ACTION_LOG_TYPE_RETURNED, dto);
		return getConsultingDisclosure(consultDisclCommonDto.getDisclosureId());
	}

	@Override
	public ResponseEntity<Object> assignAdminConsultingDisclosure(ConsultDisclAssignAdminDto consultDisclAssignAdminDto) {
		if ((consultDisclAssignAdminDto.getActionType().equals("R") && consultingDisclosureDao.isSameAdminPersonOrGroupAdded(consultDisclAssignAdminDto.getAdminGroupId(), consultDisclAssignAdminDto.getAdminPersonId(), consultDisclAssignAdminDto.getDisclosureId()))
				|| (consultDisclAssignAdminDto.getActionType().equals("A") && consultingDisclosureDao.isAdminPersonOrGroupAdded(consultDisclAssignAdminDto.getDisclosureId()))) {
			return new ResponseEntity<>("Admin already assigned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		if (consultDisclAssignAdminDto.getActionType().equals("A")) {
			List<String> consultingDisclStatuses = Arrays.asList(Constants.CONSULT_DISCL_STATUS_SUBMIT);
			if (!consultingDisclosureDao.isConsultingDisclWithStatuses(consultingDisclStatuses, null, consultDisclAssignAdminDto.getDisclosureId())) {
				return new ResponseEntity<>("Assign admin action cannot be performed", HttpStatus.METHOD_NOT_ALLOWED);
			}
		}
		if (consultDisclAssignAdminDto.getActionType().equals("R")) {
			List<String> consultingDisclStatuses = Arrays.asList(Constants.CONSULT_DISCL_STATUS_RETURN, Constants.CONSULT_DISCL_STATUS_REVIEW_COMPLETED);
			if (consultingDisclosureDao.isConsultingDisclWithStatuses(consultingDisclStatuses, null, consultDisclAssignAdminDto.getDisclosureId())) {
				return new ResponseEntity<>("Reassign admin action cannot be performed", HttpStatus.METHOD_NOT_ALLOWED);
			}
		}		
		consultDisclAssignAdminDto.setDisclosureStatus(Boolean.TRUE.equals(consultingDisclosureDao.isAdminAssigned(consultDisclAssignAdminDto.getDisclosureId())) 
						? null
						: Constants.CONSULT_DISCL_STATUS_REVIEW_IN_PROGRESS);
		try {
			saveAssignAdminActionLog(consultDisclAssignAdminDto.getAdminPersonId(), consultDisclAssignAdminDto.getDisclosureId());
		} catch (Exception e) {
			logger.error("Exception in assignAdminConsultingDisclosure : {}", e.getMessage());
		}
		consultingDisclosureDao.assignAdminConsultingDisclosure(consultDisclAssignAdminDto);
		return getConsultingDisclosure(consultDisclAssignAdminDto.getDisclosureId());
	}

	private Boolean saveAssignAdminActionLog(String adminPersonId, Integer disclosureId) {
		Boolean isAdminAssigned = consultingDisclosureDao.isAdminAssigned(disclosureId);
		if (Boolean.TRUE.equals(isAdminAssigned)) {
			ConsultDisclCommonDto consultDisclCommonDto = ConsultDisclCommonDto.builder()
					.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
					.adminPersonName(personDao.getPersonFullNameByPersonId(consultingDisclosureDao.getAssignedAdmin(disclosureId)))
					.reassignedAdminPersonName(personDao.getPersonFullNameByPersonId(adminPersonId))
					.disclosureId(disclosureId).build();
			actionLogService.saveConsultingDisclActionLog(Constants.CONSULT_DISCL_ACTION_LOG_TYPE_ADMIN_REASSIGNED, consultDisclCommonDto);
		} else {
			ConsultDisclCommonDto consultDisclCommonDto = ConsultDisclCommonDto.builder()
					.updateUserFullName(AuthenticatedUser.getLoginUserFullName()).disclosureId(disclosureId)
					.adminPersonName(personDao.getPersonFullNameByPersonId(adminPersonId))
					.build();
			actionLogService.saveConsultingDisclActionLog(Constants.CONSULT_DISCL_ACTION_LOG_TYPE_ADMIN_ASSIGNED, consultDisclCommonDto);
		}
		return isAdminAssigned;
	}

	@Override
	public ResponseEntity<Object> completeConsultingDisclosure(Integer disclosureId) {
		List<String> consultingDisclStatuses = Arrays.asList(Constants.CONSULT_DISCL_STATUS_COMPLETED);
		if(consultingDisclosureDao.isConsultingDisclWithStatuses(consultingDisclStatuses, Constants.CONSULT_DISCL_DISPOSITION_STATUS_COMPLETED, disclosureId)) {
			return new ResponseEntity<>("Already approved", HttpStatus.METHOD_NOT_ALLOWED);
		}
		consultingDisclosureDao.completeConsultingDisclosure(disclosureId);
		ConsultDisclCommonDto  consultDisclCommonDto = ConsultDisclCommonDto.builder()
				.updateUserFullName(AuthenticatedUser.getLoginUserFullName())
				.disclosureId(disclosureId)
				.build();
		actionLogService.saveConsultingDisclActionLog(Constants.CONSULT_DISCL_ACTION_LOG_TYPE_APPROVED, consultDisclCommonDto);
		return getConsultingDisclosure(disclosureId);
	}

	@Override
	public ResponseEntity<Object> getConsultingDisclosure(Integer discloureId) {
		ConsultingDisclosure consultingDisclosure = consultingDisclosureDao.getConsultingDisclosure(discloureId);
		consultingDisclosure.setUpdateUserFullName(personDao.getUserFullNameByUserName(consultingDisclosure.getUpdateUser()));
		consultingDisclosure.setAdminGroupName(consultingDisclosure.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(consultingDisclosure.getAdminGroupId()).getAdminGroupName() : null);
		consultingDisclosure.setAdminPersonName(consultingDisclosure.getAdminPersonId() != null ? personDao.getPersonFullNameByPersonId(consultingDisclosure.getAdminPersonId()) : null);
		consultingDisclosure.setHomeUnitName(commonDao.getUnitName(consultingDisclosure.getHomeUnit()));
		List<ConsultingDisclFormBuilderDetails> consultingDisclFormBuilderDetails = consultingDisclosureDao.getConsultDisclFormBuilderDetailsByDisclosureId(discloureId);
		consultingDisclosure.setConsultingDisclFormBuilderDetails(consultingDisclFormBuilderDetails);
		consultingDisclosure.setPersonAttachmentsCount(conflictOfInterestDao.personAttachmentsCount(consultingDisclosure.getPersonId()));
		consultingDisclosure.setPersonNotesCount(conflictOfInterestDao.personNotesCount(consultingDisclosure.getPersonId()));
		consultingDisclosure.setPersonEntitiesCount(conflictOfInterestDao.getSFIOfDisclosureCount(ConflictOfInterestVO.builder().personId(consultingDisclosure.getPersonId()).build()));
		return new ResponseEntity<>(consultingDisclosure, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> savePersonEntityDetails(ConsultDisclCommonDto dto) {
		consultingDisclosureDao.savePersonEntityDetails(dto.getPersonEntityId(), dto.getDisclosureId(), dto.getEntityNumber());
		return getConsultingDisclosure(dto.getDisclosureId());
	}

}

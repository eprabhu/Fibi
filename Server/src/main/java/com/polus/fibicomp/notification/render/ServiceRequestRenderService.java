package com.polus.fibicomp.notification.render;

import java.util.Map;

import org.apache.commons.collections4.map.HashedMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;

@Transactional
@Service
public class ServiceRequestRenderService implements EmailRenderService {

	@Autowired
	public ServiceRequestDao serviceRequestDao;

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	public AwardDao awardDao;

	@Autowired
	public AwardRenderService awardRenderService;

	@Autowired
	public PersonDao personDao;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		Integer id = Integer.parseInt(moduleItemKey);
		ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(id);
		Map<String, String> placeHolder = getServiceRequestPlaceHolder(serviceRequest);
		if (Boolean.TRUE.equals(serviceRequest.getIsSystemGenerated())) {
			placeHolder.putAll(awardRenderService.getPlaceHolderData(serviceRequest.getOriginatingModuleItemKey() !=null ? serviceRequest.getOriginatingModuleItemKey() : serviceRequest.getModuleItemKey()));
		}
		return placeHolder;
	}

	public Map<String, String> getServiceRequestPlaceHolder(ServiceRequest serviceRequest) {
		Map<String, String> placeHolder = new HashedMap<>();
		placeHolder.put("{SERVICE_REQUEST_ID}", serviceRequest.getServiceRequestId() == null ? "" : serviceRequest.getServiceRequestId().toString());
		placeHolder.put("{DESCRIPTION}", serviceRequest.getDescription() == null ? "" : serviceRequest.getDescription());
		placeHolder.put("{ASSIGNEE}", serviceRequest.getAssigneePersonId() != null ? personDao.getPersonFullNameByPersonId(serviceRequest.getAssigneePersonId()) : "");
		placeHolder.put("{REPORTER_USER_NAME}", serviceRequest.getReporterPersonId() != null ? personDao.getPersonFullNameByPersonId(serviceRequest.getReporterPersonId()) : "");
		placeHolder.put("{SERVICE_TYPE}", serviceRequest.getServiceRequestType() == null ? "" : serviceRequest.getServiceRequestType().getDescription());
		placeHolder.put("{SERVICE_STATUS}", serviceRequest.getServiceRequestStatus() == null ? "" : serviceRequest.getServiceRequestStatus().getDescription());
		placeHolder.put("{PREVIOUS_SERVICE_STATUS}", serviceRequestDao.getPreviousServiceRequestStatus(serviceRequest.getServiceRequestId(), serviceRequest.getStatusCode()));
		placeHolder.put("{SUBJECT}", serviceRequest.getSubject() == null ? "" : serviceRequest.getSubject());
		placeHolder.put("{DEPARTMENT}", serviceRequest.getUnitNumber() != null ?  serviceRequest.getUnit().getUnitName() : "");
		placeHolder.put("{MODULE_ITEM_KEY}", serviceRequest.getModuleItemKey() == null ? "" : serviceRequest.getModuleItemKey());
		placeHolder.put("{MODULE_NAME}", serviceRequest.getServiceRequestModule() != null ? serviceRequest.getServiceRequestModule().getDescription() : "");
		placeHolder.put("{CREATE_USER}", serviceRequest.getCreateUser() != null ? personDao.getUserFullNameByUserName(serviceRequest.getCreateUser()) : "");
		placeHolder.put("{ADMIN_GROUP_NAME}", serviceRequest.getAdminGroupId() != null ?  serviceRequest.getAdminGroup().getAdminGroupName() : "");
		placeHolder.put("{APPLICATION_URL}", generateLinkToApplication(serviceRequest.getServiceRequestId()));
		placeHolder.put("{SERVICE_REQUEST_APPLICATION_URL}", generateLinkToServiceRequest(serviceRequest.getServiceRequestId()));
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return Constants.SERVICE_REQUEST_MODULE_CODE.toString();
	}

	@Override
	public String getSubModuleCode() {
		return "0";
	}

	public String generateLinkToApplication(Integer parameter) {
		ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(parameter);
		String url = null;
		if (serviceRequest.getModuleCode() != null) {
			if (serviceRequest.getModuleCode().equals(Constants.AWARD_MODULE_CODE)) {
				if (serviceRequest.getOriginatingModuleItemKey() != null) {
					url = Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_AWARD_PATH + serviceRequest.getOriginatingModuleItemKey() + Constants.APPLICATION_URL_END_TAG;
				} else if (serviceRequest.getModuleItemKey() != null) {
					Award award = awardDao.fetchAwardByAwardId(serviceRequest.getModuleItemKey());
					Award nextAward = awardDao.fetchAwardByParams(award.getAwardNumber(), award.getSequenceNumber() + 1);
					if (nextAward != null) {
						url = Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_AWARD_PATH + nextAward.getAwardId() + Constants.APPLICATION_URL_END_TAG;
					}
				}
			} else if (serviceRequest.getModuleCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
				if (serviceRequest.getModuleItemKey() != null) {
					url = Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_PROPOSAL_PATH + Integer.parseInt(serviceRequest.getModuleItemKey()) + Constants.APPLICATION_URL_END_TAG;
				}	
			}
		}
		return url;
	}

	public String generateLinkToServiceRequest(Integer parameter) {
		return Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_SERVICE_REQUEST_PATH + parameter + Constants.SERVICE_REQUEST_APPLICATION_URL_END_TAG;
	}

}

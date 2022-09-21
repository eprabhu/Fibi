package com.polus.fibicomp.person.delegation.delegationService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.delegation.delegationDao.DelegationDao;
import com.polus.fibicomp.person.delegation.pojo.Delegations;
import com.polus.fibicomp.person.delegation.vo.DelegationVO;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "delegationService")
public class DelegationServiceImpl implements DelegationService {

protected static Logger logger = LogManager.getLogger(DelegationServiceImpl.class.getName());
	
	@Autowired
	private PersonDao personDao;

	@Autowired
	private DelegationDao delegationDao;

	@Autowired
	private CommonDao commonDao;
	
	@Autowired
	private CommonService commonService;

	@Autowired
	private InboxService inboxService;

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	private EmailService emailService;

	@Override
	public String saveOrUpdateDeligation(DelegationVO vo) {
		Delegations delegation = vo.getDelegation();
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		String previousDelegatedByPersonId = delegation.getDelegatedBy();
		String previousDelegatedToPersonId = vo.getPreviousDelegatedToPersonId();
		Boolean delegationRequest = Boolean.FALSE;
		Boolean isMaintainDelegationRightExist = Boolean.FALSE;
		String unitNumber = personDao.getPersonUnitNumberByPersonId(vo.getPersonId());
		if (unitNumber != null) {
			isMaintainDelegationRightExist = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.MAINTAIN_DELEGATION_RIGHT_NAME, unitNumber);	
			vo.setIsMaintainDelegationRightExist(isMaintainDelegationRightExist);
		}
		if (delegation.getDelegationId() == null) {
			delegationRequest = Boolean.TRUE;
		}
		if (previousDelegatedByPersonId != null && previousDelegatedToPersonId != null && !previousDelegatedToPersonId.equals(delegation.getDelegatedTo())) {
			inboxDao.markReadMessage(Constants.PERSON_MODULE_CODE, previousDelegatedToPersonId, null, Constants.MESSAGE_TYPE_DELEGATION_REQUESTED, delegation.getDelegationId().toString(), Constants.PERSON_SUBMODULE_CODE);
			delegationRequest = Boolean.TRUE;
		}
		delegation = delegationDao.saveOrUpdateDelegation(delegation);
		Person delegatedByPersonDetail = personDao.getPersonDetailById(delegation.getDelegatedBy());
		Person createPerson = personDao.getPersonDetailByPrincipalName(delegation.getCreateUser());
		if (delegation.getDelegationStatusCode().equals(Constants.DELEGATION_REQUESTED_STATUS) 
				|| delegation.getDelegationStatusCode().equals(Constants.DELEGATION_ACCEPTED_STATUS)
				|| delegation.getDelegationStatusCode().equals(Constants.DELEGATION_DENIED_STATUS)) {
			prepareDdelegationRequest(delegation, delegationRequest);
			commonService.setNotificationRecipients(delegation.getDelegatedTo(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			commonService.setNotificationRecipients(createPerson.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			StringBuilder createUserDetail = new StringBuilder();
			String createUserName = createPerson.getFullName();
			if (Boolean.TRUE.equals(isMaintainDelegationRightExist) && !delegation.getDelegatedBy().equals(AuthenticatedUser.getLoginPersonId())
					&& !createUserName.equals(delegatedByPersonDetail.getFullName())) {
				createUserDetail.append(createUserName).append(" on behalf of ").append(delegatedByPersonDetail.getFullName());
			} else if (Boolean.TRUE.equals(isMaintainDelegationRightExist) && createUserName.equals(delegatedByPersonDetail.getFullName()) && !delegation.getDelegatedBy().equals(AuthenticatedUser.getLoginPersonId())) {
				createUserDetail.append(AuthenticatedUser.getLoginUserFullName()).append(" on behalf of ").append(delegatedByPersonDetail.getFullName());
			} else if (delegation.getDelegatedBy().equals(AuthenticatedUser.getLoginPersonId())) {
				createUserDetail.append(delegatedByPersonDetail.getFullName());
			}
			sendDelegationNotification(delegation.getDelegatedTo(), delegation.getDelegatedBy(), Constants.DELEGATION_REQUEST_NOTIFICATION_CODE, dynamicEmailRecipients, createUserDetail.toString());			
		}
		if (delegation.getDelegationStatusCode().equals(Constants.DELEGATION_DELETE_STATUS)) {
			inboxDao.markAsExpiredFromActionList(Constants.PERSON_MODULE_CODE, delegation.getDelegatedTo(), Constants.PERSON_SUBMODULE_CODE, delegation.getDelegationId().toString());
			commonService.setNotificationRecipients(delegation.getDelegatedTo(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			commonService.setNotificationRecipients(createPerson.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			sendDelegationNotification(delegation.getDelegatedTo(), vo.getPersonId(), Constants.DELEGATION_REMOVED_NOTIFICATION_CODE, dynamicEmailRecipients, null);
		}
		delegation.setDelegationStatus(delegationDao.loadDelegationStatusById(delegation.getDelegationStatusCode()));
		delegation.setDelegatedByPerson(delegatedByPersonDetail);
		delegation.setDelegatedToPerson(personDao.getPersonDetailById(delegation.getDelegatedTo()));
		vo.setDelegation(delegation);
		List<Delegations> delegations = delegationDao.getDelegationByPersonId(vo.getPersonId());
		if(delegations != null && !delegations.isEmpty()) {
		 getFullNameOfUpdateUser(delegations);
		 vo.setDelegations(delegations);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void prepareDdelegationRequest(Delegations delegation, Boolean delegationRequest) {
		if (Boolean.FALSE.equals(delegationRequest)) {
			inboxDao.updateMessageInboxByParam(delegation.getDelegationId().toString(), Constants.PERSON_MODULE_CODE, Constants.MESSAGE_TYPE_DELEGATION_REQUESTED);
		} else {
			delegationRequest(delegation);
		}
	}

	private void delegationRequest(Delegations delegation) {
		String userMessage = "Delegation requested by " + personDao.getPersonFullNameByPersonId(delegation.getDelegatedBy());
		inboxService.addDelegationMessageToInbox(delegation.getDelegationId().toString(), delegation.getDelegatedTo(),
				AuthenticatedUser.getLoginUserName(), Constants.MESSAGE_TYPE_DELEGATION_REQUESTED, "D", userMessage);
	}

	private void sendDelegationNotification(String toPersonId, String personId, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients, String createUserDetail) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleItemKey(personId);
		emailServiceVO.setModuleCode(Constants.PERSON_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.PERSON_SUBMODULE_CODE.toString());
		emailServiceVO.setPlaceHolder(getDelegationPlaceholders(toPersonId, createUserDetail));
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
	}

	private Map<String, String> getDelegationPlaceholders(String toPersonId, String createUserDetail) {
		Map<String, String> placeHolder = new HashMap<>();
		Person person = personDao.getPersonDetailById(toPersonId);
		placeHolder.put("{USER_NAME}", person.getFullName() != null ? person.getFullName() : "");
		placeHolder.put("{CREATE_USER_NAME}", createUserDetail != null ? createUserDetail : "");
		placeHolder.put("{LOGIN_USER_NAME}", AuthenticatedUser.getLoginUserFullName() != null ? AuthenticatedUser.getLoginUserFullName() : "");
		return placeHolder;
	}

	@Override
	public String loadDelegationByPersonId(String delegationPersonId) {
		DelegationVO vo = new DelegationVO();
		Boolean isMaintainDelegationRightExist = Boolean.FALSE;
		String unitNumber = personDao.getPersonUnitNumberByPersonId(delegationPersonId);
		if (unitNumber != null) {
			isMaintainDelegationRightExist = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.MAINTAIN_DELEGATION_RIGHT_NAME, unitNumber);	
			vo.setIsMaintainDelegationRightExist(isMaintainDelegationRightExist);
		}
		List<String> delegationStatusCode = new ArrayList<>();
		delegationStatusCode.add(Constants.DELEGATION_ACCEPTED_STATUS);
		delegationStatusCode.add(Constants.DELEGATION_REQUESTED_STATUS);
		delegationStatusCode.add(Constants.DELEGATION_DENIED_STATUS);
		Delegations delegation = delegationDao.loadDelegationByParams(delegationPersonId, delegationStatusCode);
		if (delegation != null && (isMaintainDelegationRightExist || (delegationPersonId.equals(AuthenticatedUser.getLoginPersonId())))) {
			vo.setDelegation(delegation);
		}
		List<Delegations> delegations = delegationDao.getDelegationByPersonId(delegationPersonId);
		if(delegations != null && !delegations.isEmpty()) {
		 getFullNameOfUpdateUser(delegations);
		 vo.setDelegations(delegations);
		}
		Person person = personDao.getPersonDetailById(delegationPersonId);
		if (person != null) {
			vo.setPerson(person);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void getFullNameOfUpdateUser(List<Delegations> delegations) {
		Set<String> user = delegations.stream().map(Delegations::getCreateUser).collect(Collectors.toSet());
		user.addAll(delegations.stream().map(Delegations::getUpdateUser).collect(Collectors.toSet()));
		if (!user.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(user));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			delegations.stream().filter(item -> item.getCreateUser() != null).filter(item -> collect.containsKey(item.getCreateUser().toUpperCase())).forEach(item -> item.setCreateUserFullName(collect.get(item.getCreateUser().toUpperCase())));
			delegations.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}

	@Override
	public String updateDeligationStatus(DelegationVO vo) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		Delegations delegation = delegationDao.getDelegationByDelegationId(vo.getDelegationId());
		String createPersonId = personDao.getPersonIdByUserName(delegation.getCreateUser());
		if (vo.getDelegationStatusCode().equals(Constants.DELEGATION_ACCEPTED_STATUS)) {
			delegation.setDelegationStatusCode(vo.getDelegationStatusCode());
			delegation = delegationDao.saveOrUpdateDelegation(delegation);
			commonService.setNotificationRecipients(createPersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			inboxDao.markReadMessage(Constants.PERSON_MODULE_CODE, delegation.getDelegatedTo() , null, Constants.MESSAGE_TYPE_DELEGATION_REQUESTED, delegation.getDelegationId().toString(), Constants.PERSON_SUBMODULE_CODE);
			commonService.setNotificationRecipients(delegation.getDelegatedBy(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			sendDelegationNotification(delegation.getDelegatedBy(), vo.getPersonId(), Constants.DELEGATION_ACCEPT_NOTIFICATION_CODE, dynamicEmailRecipients, null);
		} else if (vo.getDelegationStatusCode().equals(Constants.DELEGATION_DENIED_STATUS)) {
			inboxDao.markReadMessage(Constants.PERSON_MODULE_CODE, delegation.getDelegatedTo() , null, Constants.MESSAGE_TYPE_DELEGATION_REQUESTED, delegation.getDelegationId().toString(), Constants.PERSON_SUBMODULE_CODE);
			delegation.setDelegationStatusCode(vo.getDelegationStatusCode());
			delegation = delegationDao.saveOrUpdateDelegation(delegation);
			commonService.setNotificationRecipients(delegation.getDelegatedBy(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			commonService.setNotificationRecipients(createPersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			sendDelegationNotification(delegation.getDelegatedBy(), vo.getPersonId(), Constants.DELEGATION_DENIED_NOTIFICATION_CODE, dynamicEmailRecipients, null);
		} else if (vo.getDelegationStatusCode().equals(Constants.DELEGATION_DELETE_STATUS)) {
			inboxDao.markReadMessage(Constants.PERSON_MODULE_CODE, delegation.getDelegatedTo() , null, Constants.MESSAGE_TYPE_DELEGATION_REQUESTED, delegation.getDelegationId().toString(), Constants.PERSON_SUBMODULE_CODE);
			delegation.setDelegationStatusCode(vo.getDelegationStatusCode());
			delegation = delegationDao.saveOrUpdateDelegation(delegation);
			commonService.setNotificationRecipients(delegation.getDelegatedBy(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			commonService.setNotificationRecipients(createPersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			sendDelegationNotification(delegation.getDelegatedBy(), vo.getPersonId(), Constants.DELEGATION_REMOVED_NOTIFICATION_CODE, dynamicEmailRecipients, null);
		}
		delegation.setDelegationStatus(delegationDao.loadDelegationStatusById(delegation.getDelegationStatusCode()));
		List<Delegations> delegations = delegationDao.getDelegationByPersonId(vo.getPersonId());
		if(delegations != null && !delegations.isEmpty()) {
		 getFullNameOfUpdateUser(delegations);
		 vo.setDelegations(delegations);
		}
		return commonDao.convertObjectToJSON(vo);
	}

}

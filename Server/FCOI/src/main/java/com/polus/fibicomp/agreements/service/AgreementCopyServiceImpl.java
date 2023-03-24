package com.polus.fibicomp.agreements.service;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.pojo.AgreementClauses;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.agreements.pojo.AgreementSponsor;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorContact;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.roles.service.AuthorizationService;

@Transactional
@Service(value = "agreementCopyService")
public class AgreementCopyServiceImpl  implements AgreementCopyService{

	protected static Logger logger = LogManager.getLogger(AgreementCopyServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AgreementDao agreementDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private AgreementService agreementService;

	@Autowired
	private AuthorizationService authorizationService;

	@Override
	public AgreementVO copyAgreement(AgreementVO vo) {
		AgreementHeader orginalAgreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		if (orginalAgreementHeader != null) {
			AgreementHeader copyAgreementHeader = new AgreementHeader();
			copyAgreementDetails(vo, orginalAgreementHeader, copyAgreementHeader);
			agreementService.loadInitialData(vo);
			AgreementHeader agreementHeader = vo.getAgreementHeader();
			agreementService.addActionLogEntry(agreementHeader.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_CREATED, agreementHeader.getUpdateUser(), null);
			agreementHeader.setNegotiationId(agreementService.createNewNegotiation(agreementHeader).getNegotiationId());
			List<AgreementClauses> agreementClauses = agreementService.addAgreementClausesBasedOnAgreementType(agreementHeader.getAgreementTypeCode(), agreementHeader.getAgreementRequestId(), agreementHeader.getUpdateUser());
			if (agreementClauses != null && !agreementClauses.isEmpty()) {
				vo.setAgreementClausesGroup(agreementService.prepareAgreementClauses(agreementClauses));
			}
			//Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			vo.setAgreementRequestId(agreementHeader.getAgreementRequestId());
			if (agreementHeader.getUnitNumber() != null) {
				vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AGREEMENT_MODULE_CODE, agreementHeader.getRequestorPersonId(), agreementHeader.getUnitNumber(), agreementHeader.getAgreementRequestId()));
			} else {
				vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AGREEMENT_MODULE_CODE, agreementHeader.getRequestorPersonId(), Constants.ROOT_UNIT, agreementHeader.getAgreementRequestId()));
			}
			agreementService.loadAgreementUserFullNames(vo.getAgreementHeader());
		}
		//agreementService.sendNotificationForAgreement(vo, Constants.CREATE_AGREEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
		return vo;
	}

	private AgreementVO copyAgreementDetails(AgreementVO vo, AgreementHeader orginalAgreementHeader,
			AgreementHeader copyAgreementHeader) {
		Person person = personDao.getPersonDetailByPrincipalName(vo.getUpdateUser());
		agreementService.updateAgreementStatus(copyAgreementHeader, Constants.AGREEMENT_STATUS_INPROGRESS);
		copyAgreementHeader.setAgreementCategory(orginalAgreementHeader.getAgreementCategory());
		copyAgreementHeader.setCategoryCode(orginalAgreementHeader.getCategoryCode());
		copyAgreementHeader.setAgreementType(orginalAgreementHeader.getAgreementType());
		copyAgreementHeader.setAgreementTypeCode(orginalAgreementHeader.getAgreementTypeCode());
		copyAgreementHeader.setStartDate(orginalAgreementHeader.getStartDate());
		copyAgreementHeader.setEndDate(orginalAgreementHeader.getEndDate());
		copyAgreementHeader.setTitle(orginalAgreementHeader.getTitle());
		copyAgreementHeader.setRemarks(orginalAgreementHeader.getRemarks());
		copyAgreementHeader.setContractValue(orginalAgreementHeader.getContractValue());
		if (person != null) {
			copyAgreementHeader.setRequestorPersonId(person.getPersonId());
			copyAgreementHeader.setRequestorName(person.getFullName());
			copyAgreementHeader.setUnitNumber(person.getHomeUnit());
			if (person.getHomeUnit() != null) {
				Unit unit = commonDao.getUnitByUnitNumber(person.getHomeUnit());
				if (unit != null) {
					copyAgreementHeader.setUnit(commonDao.getUnitByUnitNumber(person.getHomeUnit()));
				}
			}
		} else {
			copyAgreementHeader.setUnitNumber(orginalAgreementHeader.getUnitNumber());
			copyAgreementHeader.setUnit(orginalAgreementHeader.getUnit());
		}
		agreementService.loadPersonDetailsInAgreement(copyAgreementHeader);
		copyAgreementHeader.setUnitNumber(orginalAgreementHeader.getUnitNumber());
		copyAgreementHeader.setUnit(orginalAgreementHeader.getUnit());
		copyAgreementHeader.setUnitName(orginalAgreementHeader.getUnitName());
		copyAgreementHeader.setCreateTimeStamp(commonDao.getCurrentTimestamp());
		copyAgreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		copyAgreementHeader.setCreateUser(vo.getUpdateUser());
		copyAgreementHeader.setUpdateUser(vo.getUpdateUser());
		copyAgreementHeader.setCurrencyCode(orginalAgreementHeader.getCurrencyCode());
		copyAgreementHeader.setCurrency(orginalAgreementHeader.getCurrency());
		copyAgreementHeader.setAmountInWords(orginalAgreementHeader.getAmountInWords());
		copyAgreementHeader = agreementDao.saveOrUpdateAgreement(copyAgreementHeader);
		List<AgreementSponsor> orginalAgreementSponsors = agreementService.getAgreementSponsors(orginalAgreementHeader.getAgreementRequestId());
		if (orginalAgreementSponsors != null && !orginalAgreementSponsors.isEmpty()) {
			copyAgreementSponsors(vo,orginalAgreementSponsors, copyAgreementHeader.getAgreementRequestId());
		}
		List <AgreementPeople> agreementPeoples = agreementDao.getAllAgreementPeople(orginalAgreementHeader.getAgreementRequestId());
		if (agreementPeoples != null && !agreementPeoples.isEmpty()) {
			copyAgreementPeople(vo,agreementPeoples, copyAgreementHeader.getAgreementRequestId());
		}
		vo.setAgreementHeader(copyAgreementHeader);
		vo.setAgreementClausesGroup(agreementService.prepareAgreementClauses(agreementDao.fetchAgreementClausesBasedOnAgreementId(copyAgreementHeader.getAgreementRequestId())));
		return vo;
	}

	private void copyAgreementPeople(AgreementVO vo, List<AgreementPeople> agreementPeoples, Integer agreementRequestId) {
		List<AgreementPeople> newAgreementPeoples = new ArrayList<>();
		for (AgreementPeople agreementPeople : agreementPeoples) {
			AgreementPeople newAgreementPeople = new AgreementPeople();
			newAgreementPeople.setPeopleTypeId(agreementPeople.getPeopleTypeId());
			newAgreementPeople.setRolodexId(agreementPeople.getRolodexId());
			newAgreementPeople.setAgreementPeopleType(agreementPeople.getAgreementPeopleType());
			newAgreementPeople.setPersonId(agreementPeople.getPersonId());
			newAgreementPeople.setFullName(agreementPeople.getFullName());
			newAgreementPeople.setAgreementRequestId(agreementRequestId);
			newAgreementPeople.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newAgreementPeople.setUpdateUser(vo.getUpdateUser());
			if (agreementPeople.getPiPersonnelTypeCode() != null) {
				newAgreementPeople.setPiPersonnelTypeCode(agreementPeople.getPiPersonnelTypeCode());
				newAgreementPeople.setPiPersonnelType(agreementPeople.getPiPersonnelType());
			}
			newAgreementPeople = agreementDao.saveOrUpdateAgreementPeople(newAgreementPeople);
			newAgreementPeoples.add(newAgreementPeople);
		}
		vo.setAgreementPeoples(agreementService.preparePeopleDetails(newAgreementPeoples));
	}

	private void copyAgreementSponsors(AgreementVO vo, List<AgreementSponsor> orginalAgreementSponsors, Integer agreementRequestId) {
		List<AgreementSponsor> newAgreementSponsors = new ArrayList<>();
		for (AgreementSponsor agreementSponsor : orginalAgreementSponsors) {
			AgreementSponsor newAgreementSponsor = new AgreementSponsor();
			newAgreementSponsor.setAgreementRequestId(agreementRequestId);
			newAgreementSponsor.setAddress(agreementSponsor.getAddress());
			newAgreementSponsor.setRegistrationNumber(agreementSponsor.getRegistrationNumber());
			newAgreementSponsor.setSponsorName(agreementSponsor.getSponsorName());
			newAgreementSponsor.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newAgreementSponsor.setSponsorCode(agreementSponsor.getSponsorCode());
			newAgreementSponsor.setSponsor(agreementSponsor.getSponsor());
			newAgreementSponsor.setSponsorRole(agreementSponsor.getSponsorRole());
			newAgreementSponsor.setSponsorRoleTypeCode(agreementSponsor.getSponsorRoleTypeCode());
			newAgreementSponsor.setAgreementSponsorType(agreementSponsor.getAgreementSponsorType());
			newAgreementSponsor.setAgreementSponsorTypeCode(agreementSponsor.getAgreementSponsorTypeCode());
			newAgreementSponsor.setUpdateUser(vo.getUpdateUser());
			newAgreementSponsor = agreementDao.saveOrUpdateAgreementSponsor(newAgreementSponsor);
			List<AgreementSponsorContact> agreementSponsorContacts = agreementDao.getAgreementSponsorContacts(agreementSponsor.getAgreementSponsorId());
			if (agreementSponsorContacts != null && !agreementSponsorContacts.isEmpty()) {
				copyAgreementContactList(agreementSponsorContacts, newAgreementSponsor, vo.getUpdateUser());
			}
			newAgreementSponsors.add(newAgreementSponsor);
		}
		vo.setAgreementSponsors(newAgreementSponsors);
	}

	private void copyAgreementContactList(List<AgreementSponsorContact> agreementSponsorContacts, AgreementSponsor newAgreementSponsor, String updateUser) {
		List<AgreementSponsorContact> newAgreementSponsorContacts = new ArrayList<>();
		for (AgreementSponsorContact agreementSponsorContact : agreementSponsorContacts) {
			AgreementSponsorContact newAgreementSponsorContact = new AgreementSponsorContact();
			newAgreementSponsorContact.setAgreementRequestId(newAgreementSponsor.getAgreementRequestId());
			newAgreementSponsorContact.setAgreementSponsorId(newAgreementSponsor.getAgreementSponsorId());
			if (agreementSponsorContact.getSponsorContactTypeCode() != null) {
				newAgreementSponsorContact.setSponsorContactTypeCode(agreementSponsorContact.getSponsorContactTypeCode());
				newAgreementSponsorContact.setAgreementSponsorContactType(agreementSponsorContact.getAgreementSponsorContactType());	
			}
			newAgreementSponsorContact.setContactPersonName(agreementSponsorContact.getContactPersonName());
			newAgreementSponsorContact.setContactAddressLine(agreementSponsorContact.getContactAddressLine());
			newAgreementSponsorContact.setContactCity(agreementSponsorContact.getContactCity());
			newAgreementSponsorContact.setContactEmailId(agreementSponsorContact.getContactEmailId());
			newAgreementSponsorContact.setContactPhone(agreementSponsorContact.getContactPhone());
			newAgreementSponsorContact.setContactState(agreementSponsorContact.getContactState());
			newAgreementSponsorContact.setDesignation(agreementSponsorContact.getDesignation());
			newAgreementSponsorContact.setSalutation(agreementSponsorContact.getSalutation());
			newAgreementSponsorContact.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newAgreementSponsorContact.setUpdateUser(updateUser);
			newAgreementSponsorContact = agreementDao.saveOrUpdateAgreementSponsorContact(newAgreementSponsorContact);
			newAgreementSponsorContacts.add(newAgreementSponsorContact);
		}
		newAgreementSponsor.getAgreementSponsorContacts().addAll(newAgreementSponsorContacts);
	}
}

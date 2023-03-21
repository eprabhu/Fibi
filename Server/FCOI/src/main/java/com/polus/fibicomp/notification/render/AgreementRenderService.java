package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.Map;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;

@Transactional
@Service
public class AgreementRenderService implements EmailRenderService{

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	public AgreementDao agreementDao;

	@Autowired
	public CommonService commonService;

	@Autowired
	public PersonDao personDao;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(Integer.parseInt(moduleItemKey));
		return getAgreementPlaceHolder(agreementHeader);
	}

	@Override
	public String getModuleType() {
		return Constants.AGREEMENT_MODULE_CODE.toString();
	}

	@Override
	public String getSubModuleCode() {
		return Constants.AGREEMENT_SUBMODULE_CODE.toString();
	}

	public String generateLinkToApplication(Integer parameter) {
		return Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_AGREEMENT_PATH + parameter + Constants.APPLICATION_URL_END_TAG;
	}

	public Map<String, String> getAgreementPlaceHolder(AgreementHeader agreementHeader) {
		Map<String, String> placeHolder = new HashMap<>();
		String link = generateLinkToApplication(agreementHeader.getAgreementRequestId());
		String primaryOrganization = agreementDao.getPrimaryOrganisationName(agreementHeader.getAgreementRequestId());
		String piName = "";
		String piLastName = "";
		AgreementPeople agreementPeople = agreementDao.getAgreementPI(agreementHeader.getAgreementRequestId());
		if (agreementPeople != null) {
			piName = agreementPeople.getFullName();
			if (agreementPeople.getPersonId() != null) {
				piLastName = personDao.getPersonDetailById(agreementPeople.getPersonId()).getLastName();
			}
		}
		placeHolder.put("{AGREEMENT_TITLE}", (agreementHeader.getTitle() != null) ? agreementHeader.getTitle() + "" : "");
		placeHolder.put("{AGREEMENT_ID}", (agreementHeader.getAgreementRequestId() != null) ? agreementHeader.getAgreementRequestId() + "" : "");
		placeHolder.put("{CONTRACT_VALUE}", (agreementHeader.getContractValue() != null) ? agreementHeader.getContractValue() + "" : "");
		placeHolder.put("{AGREEMENT_TYPE}", (agreementHeader.getAgreementType() != null) ? agreementHeader.getAgreementType().getDescription() + "" : "");
		placeHolder.put("{PI_FULL_NAME}", (piName != null) ? piName + "" : "");
		placeHolder.put("{PI_LAST_NAME}", (piLastName != null) ? piLastName + "" : "");
		placeHolder.put("{PRIMARY_ORGANIZATION}", (primaryOrganization != null) ? primaryOrganization + "" : "");
		placeHolder.put("{AGREEMENT_START_DATE}", (agreementHeader.getStartDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(agreementHeader.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{AGREEMENT_END_DATE}", (agreementHeader.getEndDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(agreementHeader.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{REQUESTOR_FULL_NAME}", (agreementHeader.getRequestorPersonId() != null) ? setRequestorFullName(agreementHeader.getRequestorPersonId()) : "");
		placeHolder.put("{APPLICATION_URL}", link);
		placeHolder.put("{LEAD_UNIT_NAME}", (agreementHeader.getUnitName() != null) ? agreementHeader.getUnitName() + "" : "");
		return placeHolder;
	}

	private String setRequestorFullName(String requestorPersonId) {
		return personDao.getPersonDetailById(requestorPersonId).getFullName();
	}

}

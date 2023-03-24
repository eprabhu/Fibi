package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;


@Transactional
@Service
public class ProposalRenderService implements EmailRenderService {

	@Autowired
	public ProposalDao proposalDao;

	@Autowired
	public ProposalModuleDao proposalModuleDao;

	@Autowired
	private GrantCallDao grantCallDao;
	
	@Autowired
	private CommonService commonService;

	@Value("${spring.application.name}")
	private String applicationURL;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		Integer proposalId = Integer.parseInt(moduleItemKey);
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId));
		return  getProposalPlaceHolder(proposal);
	}

	public Map<String, String> getProposalPlaceHolder(Proposal proposal) {
		Map<String, String> placeHolder = new HashMap<>();
		String link = generateLinkToApplication(proposal.getProposalId());
		String dashboardLink = generateLinkToProposalDashboard(proposal.getProposalId());
		placeHolder.put("{APPLICATION_URL_GRANT}", proposal.getGrantCallId() != null ? generateGrantCallLinkToApplication(proposal.getGrantCallId()) : "");	
		placeHolder.put("{APPLICATION_URL_PROPOSAL_PERSON_CERT}", proposal.getProposalId() != null ? generatePersonCertificationLinkToApplication(proposal.getProposalId()) : "");	
		placeHolder.put("{PROPOSAL_NUMBER}", (proposal.getProposalId()!=null) ? proposal.getProposalId() + "": "");
		placeHolder.put("{PROPOSAL_TITLE}", proposal.getTitle().replaceAll("\\s+", " "));
		placeHolder.put("{PRINCIPAL_INVESTIGATOR}", proposal.getInvestigator() != null ? proposal.getInvestigator().getFullName() + "" : "");
		placeHolder.put("{DEADLINE_DATE}", proposal.getSponsorDeadlineDate() != null ? commonService.convertDateFormatBasedOnTimeZone(proposal.getSponsorDeadlineDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{LEAD_UNIT}", proposal.getHomeUnitNumber());
		placeHolder.put("{LEAD_UNIT_NAME}", proposal.getHomeUnitName());
		placeHolder.put("{SPONSOR_NAME}", proposal.getSponsor() == null ? "" : proposal.getSponsor().getSponsorName());
		placeHolder.put("{PRINCIPAL INVESTIGATOR}", proposal.getInvestigator() != null ? proposal.getInvestigator().getFullName() + "" : "");
		if (proposal.getGrantCallId()!= null) {
			placeHolder.put("{GRANT_CALL_NAME}", grantCallDao.getGrantCallNameByGrantId(proposal.getGrantCallId())+ "");
		} else {
			placeHolder.put("{GRANT_CALL_NAME}", "");
		}
		placeHolder.put("{IP_NUMBER}", proposal.getIpNumber() != null ? proposal.getIpNumber() : "");
		placeHolder.put("{APPLICATION_ID}", proposal.getApplicationId() != null ? proposal.getApplicationId() : "");
		placeHolder.put("{APPLICATION_URL}", link);
		placeHolder.put("{DASHBOARD_URL}", dashboardLink);
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return Constants.DEV_PROPOSAL_MODULE_CODE.toString();
	}

	@Override
	public String getSubModuleCode() {
		return Constants.DEV_PROPOSAL_SUBMODULE_CODE.toString();
	}

	public String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_PROPOSAL_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

	public String generateGrantCallLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_GRANTCALL_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

	public String generatePersonCertificationLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_PROPOSAL_CERTIFIATION_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}
	
	public String generateLinkToProposalDashboard(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_PROPOSAL_DASHBOARD_PATH +Constants.APPLICATION_URL_END_TAG;
	}
}

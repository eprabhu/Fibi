package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.prereview.dao.PreReviewDao;
import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;

@Transactional
@Service
public class ProposalPreReviewRenderService implements EmailRenderService {


	@Autowired
	private PreReviewDao proposalPreReviewDao;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private ProposalModuleDao proposalModuleDao;
	
	@Autowired
	private CommonService commonService;

	@Value("${spring.application.name}")
	private String applicationURL;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		PreReview preReview = proposalPreReviewDao.getPreReviewById(Integer.parseInt(moduleItemKey));
		Map<String, String> placeHolder = getPreReviewPlaceHolder(preReview);
		return placeHolder;
	}

	public Map<String, String> getPreReviewPlaceHolder(PreReview preReview) {
		Map<String, String> placeHolder = new HashMap<String, String>();
		String link = generateLinkToApplication(Integer.parseInt(preReview.getModuleItemKey()));
		Proposal proposal = proposalDao.fetchProposalById(Integer.parseInt(preReview.getModuleItemKey()));
		proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId()));
		placeHolder.put("{PROPOSAL_NUMBER}", (proposal.getProposalId()!=null) ? proposal.getProposalId() + "": "");
		placeHolder.put("{PROPOSAL_TITLE}", proposal.getTitle().replaceAll("\\s+", " "));
		placeHolder.put("{PRINCIPAL_INVESTIGATOR}", proposal.getInvestigator() != null ? proposal.getInvestigator().getFullName() + "" : "");
		placeHolder.put("{DEADLINE_DATE}", proposal.getSponsorDeadlineDate() != null ? commonService.convertDateFormatBasedOnTimeZone(proposal.getSponsorDeadlineDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{PRE_REVIEW_TYPE}", (preReview.getPreReviewSectionType() != null) ? preReview.getPreReviewSectionType().getDescription() + "" : "");
		placeHolder.put("{APPLICATION_URL}", link);
		return placeHolder;
	}

	public String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_PROPOSAL_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

	@Override
	public String getModuleType() {
		return Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL.toString();
	}

	@Override
	public String getSubModuleCode() {
		return Constants.PROPOSAL_PRE_REVIEW_SUBMODULE_CODE.toString();
	}

}

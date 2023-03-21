package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.prereview.dao.PreReviewDao;
import com.polus.fibicomp.prereview.pojo.PreReview;

@Transactional
@Service
public class AwardPreReviewRenderService implements EmailRenderService {


	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private PreReviewDao proposalPreReviewDao;

	@Autowired
	private AwardDao awardDao;
	
	@Autowired
	private CommonService commonService;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		PreReview preReview = proposalPreReviewDao.getPreReviewById(Integer.parseInt(moduleItemKey));
		Map<String, String> placeHolder = getPreReviewPlaceHolder(preReview);
		return placeHolder;
	}

	public Map<String, String> getPreReviewPlaceHolder(PreReview preReview) {
		Map<String, String> placeHolder = new HashMap<String, String>();
		String link = generateLinkToApplication(Integer.parseInt(preReview.getModuleItemKey()));
		Award award = awardDao.fetchAwardByAwardId(preReview.getModuleItemKey());
		placeHolder.put("{AWARD_NUMBER}", (award.getAwardNumber() != null) ? award.getAwardNumber() + "" : "");
		placeHolder.put("{AWARD_TITLE}", award.getTitle().replaceAll("\\s+", " "));
		placeHolder.put("{PRINCIPAL_INVESTIGATOR}", award.getPrincipalInvestigator() != null ? award.getPrincipalInvestigator() + "" : "");
		placeHolder.put("{DEADLINE_DATE}", preReview.getCompletionDate() != null ? commonService.convertDateFormatBasedOnTimeZone(preReview.getCompletionDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{PRE_REVIEW_TYPE}", (preReview.getPreReviewSectionType() != null) ? preReview.getPreReviewSectionType().getDescription() + "" : "");
		placeHolder.put("{LEAD_UNIT}", award.getLeadUnit() != null ? award.getLeadUnit().getUnitName() + "" : "");
		List<AwardFundingProposal> awardFundingProposals = awardDao.getAwardFundingProposals(Integer.parseInt(preReview.getModuleItemKey()));
		String grantCallName = "";
		if (!awardFundingProposals.isEmpty()) {
			AwardFundingProposal awardFundingProposal = awardFundingProposals.get(0);
			InstituteProposal instituteProposal = awardFundingProposal.getProposal();
			if (instituteProposal != null) {
				GrantCall grantCall = instituteProposal.getGrantCall();
				if (grantCall != null) {
					grantCallName = grantCall.getGrantCallName() != null ? grantCall.getGrantCallName() + "" : "";
				}
			}
		}
		placeHolder.put("{GRANT_CALL_NAME}", grantCallName);
		placeHolder.put("{APPLICATION_URL}", link);
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return Constants.AWARD_MODULE_CODE.toString();
	}

	@Override
	public String getSubModuleCode() {
		return Constants.AWARD_PRE_REVIEW_SUBMODULE_CODE.toString();
	}

	public String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_AWARD_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

}

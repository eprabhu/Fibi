package com.polus.fibicomp.notification.render;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.claims.dao.ClaimsDao;
import com.polus.fibicomp.claims.pojo.Claim;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;

@Transactional
@Service
public class AwardClaimRenderService implements EmailRenderService {

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private ClaimsDao claimDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardRenderService awardRenderService;
	
	@Autowired
	private CommonService commonService;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		Claim claim = claimDao.getClaim(Integer.parseInt(moduleItemKey));
		Map<String, String> placeHolder = new HashMap<>();			
		Award award = awardDao.fetchAwardByAwardId(claim.getAwardId().toString());
		placeHolder.putAll(awardRenderService.getAwardPlaceHolder(award));
		placeHolder.putAll(getClaimPlaceHolder(claim));
		return placeHolder;
	}

	private Map<String, String> getClaimPlaceHolder(Claim claim) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{TITLE}", (claim.getTitle() != null) ? claim.getTitle() + "" : "");
		placeHolder.put("{CLAIM_START_DATE}",  (claim.getStartDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(claim.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{CLAIM_END_DATE}", (claim.getEndDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(claim.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		BigDecimal amount = null;
		amount = claim.getTotalAmount();
		if (claim.getClaimNumber().contains("R"))
			amount = claimDao.getTotalAmountRequestedForClaim(claim.getClaimId());
		placeHolder.put("{CLAIM_AMOUNT}", (new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL).format(amount)));
		placeHolder.put("{CLAIM_NUMBER}", (claim.getClaimNumber() != null) ? claim.getClaimNumber() + "" : "");
		placeHolder.put("{CLAIM_SUBMISSION_DATE}", (claim.getClaimSubmissionDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(claim.getClaimSubmissionDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{USER_NAME}", personDao.getUserFullNameByUserName(claim.getUpdateUser()));
		placeHolder.put("{APPLICATION_URL}", generateLinkToApplication(claim.getClaimId()));
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return String.valueOf(Constants.CLAIM_MODULE_CODE);
	}

	@Override
	public String getSubModuleCode() {
		return Constants.CLAIM_SUBMODULE_CODE.toString();
	}

	private String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_CLAIM_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

}

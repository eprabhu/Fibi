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
import com.polus.fibicomp.grantcall.pojo.GrantCall;

@Transactional
@Service
public class GrantcallRenderService implements EmailRenderService{

	@Autowired
	public GrantCallDao grantCallDao;
	
	@Autowired
	private CommonService commonService;

	@Value("${spring.application.name}")
	private String applicationURL;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		Integer id = Integer.parseInt(moduleItemKey);
		GrantCall grantCall = grantCallDao.fetchGrantCallById(id);
		Map<String, String> placeHolder = getGrantCallPlaceHolder(grantCall);
		return placeHolder;
	}

	public Map<String, String> getGrantCallPlaceHolder(GrantCall grantCall) {
		Map<String, String> placeHolder = new HashMap<String, String>();
		String link = generateLinkToApplication(grantCall.getGrantCallId());
		if(grantCall.getSponsorFundingScheme() != null) {
			placeHolder.put("{FUNDING_AGENCY}", grantCall.getSponsorFundingScheme().getDescription() + "");
		} else {
			placeHolder.put("{FUNDING_AGENCY}", "");
		}
		placeHolder.put("{GRANT_CALL_NAME}", grantCall.getGrantCallName().replaceAll("\\s+", " "));
		placeHolder.put("{GRANT_CALL_OPENING_DATE}", commonService.convertDateFormatBasedOnTimeZone(grantCall.getOpeningDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		placeHolder.put("{GRANT_CALL_CLOSING_DATE}", commonService.convertDateFormatBasedOnTimeZone(grantCall.getClosingDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		if(grantCall.getGrantTheme() != null) {
			placeHolder.put("{GRANT_CALL_THEME}", grantCall.getGrantTheme() + "");
		} else {
			placeHolder.put("{GRANT_CALL_THEME}", "");
		}
		if(grantCall.getOtherInformation() != null) {
			placeHolder.put("{OTHER_INFORMATION}", grantCall.getOtherInformation() + "");
		} else {
			placeHolder.put("{OTHER_INFORMATION}", "");
		}
		placeHolder.put("{APPLICATION_URL}", link);
		placeHolder.put("{GRANT_CALL_ID}", grantCall.getGrantCallId() + "");
		if(grantCall.getMaximumBudget() != null) {
			placeHolder.put("{MAX_BUDGET}", grantCall.getMaximumBudget().toString());
		} else {
			placeHolder.put("{MAX_BUDGET}", "");
		}
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return String.valueOf(Constants.MODULE_CODE_GRANT_CALL);
	}

	@Override
	public String getSubModuleCode() {
		return Constants.GRANTCALL_SUBMODULE_CODE.toString();
	}

	public String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_GRANTCALL_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

}

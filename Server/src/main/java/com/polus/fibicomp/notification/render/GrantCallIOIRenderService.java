package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.Map;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.dao.GrantCallIOIDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;

@Transactional
@Service
public class GrantCallIOIRenderService implements EmailRenderService{

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private GrantCallIOIDao grantCallIOIDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		Integer id = Integer.parseInt(moduleItemKey);
		GrantCallIOIHeader grantCallIOI = grantCallIOIDao.fetchGrantIOIByIOIId(id);
		Map<String, String> placeHolder = getGrantCallPlaceHolder(grantCallIOI);
		return placeHolder;
	}

	public Map<String, String> getGrantCallPlaceHolder(GrantCallIOIHeader grantCallIOI) {
		String link = generateLinkToApplication(grantCallIOI.getGrantCallId());
		GrantCall grantCall = grantCallDao.fetchGrantCallById(grantCallIOI.getGrantCallId());
		Map<String, String> placeHolder = new HashMap<String, String>();
		placeHolder.put("{GRANT_CALL_NAME}", grantCall.getGrantCallName().replaceAll("\\s+", " "));
		placeHolder.put("{APPLICATION_URL}", link);
		placeHolder.put("{GRANT_CALL_ID}", grantCall.getGrantCallId() + "");
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return String.valueOf(Constants.MODULE_CODE_GRANT_CALL);
	}

	@Override
	public String getSubModuleCode() {
		return Constants.GRANTCALL_IOI_SUBMODULE_CODE.toString();
	}

	public String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_GRANTCALL_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

}

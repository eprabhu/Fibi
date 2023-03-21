package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.externaluser.dao.ExternalUserDao;
import com.polus.fibicomp.externaluser.pojo.ExternalUser;

@Transactional
@Service
public class ExternalUserRenderService implements EmailRenderService {

	@Autowired
	private ExternalUserDao externalUserDao;

	@Autowired
	private CommonDao commonDao;

	@Override
	public Map<String, String> getPlaceHolderData(String personId) {
		ExternalUser externalUser = externalUserDao.getExternalUserByPersonId(Integer.parseInt(personId));
		return getExternalUserPlaceHolder(externalUser);
	}

	private Map<String, String> getExternalUserPlaceHolder(ExternalUser externalUser) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{LOGIN_URL}", commonDao.getParameterValueAsString(Constants.EXTERNAL_USER_SIGNIN_LINK));
		placeHolder.put("{FULL_NAME}", externalUser.getFullName() != null ? externalUser.getFullName() : "");
		placeHolder.put("{USER_NAME}", externalUser.getUserName() != null ? externalUser.getUserName() : "");
		placeHolder.put("{EMAIL_ADDRESS}", externalUser.getEmailAddress() != null ? externalUser.getEmailAddress() : "");
		placeHolder.put("{FUNDING_OFFICE}", externalUser.getFundingOffice() != null ? commonDao.getUnitByUnitNumber(externalUser.getFundingOffice()).getUnitName() : "");
		placeHolder.put("{ORGANIZATION_NAME}", externalUser.getOrganizationId() != null ? commonDao.loadOrganizationDetails(externalUser.getOrganizationId()).getOrganizationName() : "");
		placeHolder.put("{PASSWORD}", externalUser.getPassword() != null ? externalUser.getPassword() : "");
		placeHolder.put("{COMMENTS}", externalUser.getAdminComment() != null ? externalUser.getAdminComment() : "");
		placeHolder.put("{PASSWORD_RESET_URL}",commonDao.getParameterValueAsString(Constants.EXTERNAL_USER_PASSWORD_RESET_LINK));
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return String.valueOf(Constants.EXTERNAL_USER_MODULE_CODE);
	}

	@Override
	public String getSubModuleCode() {
		return Constants.EXTERNAL_USER_SUB_MODULE_CODE.toString();
	}

}

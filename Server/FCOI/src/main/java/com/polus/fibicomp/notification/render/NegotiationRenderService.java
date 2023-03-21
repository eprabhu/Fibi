package com.polus.fibicomp.notification.render;

import java.util.Map;

import javax.transaction.Transactional;

import org.apache.commons.collections4.map.HashedMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.negotiation.dao.NegotiationAgreementDao;
import com.polus.fibicomp.negotiation.dao.NegotiationDao;
import com.polus.fibicomp.negotiation.pojo.Negotiations;

@Transactional
@Service
public class NegotiationRenderService implements EmailRenderService{

	@Autowired
	public CommonService commonService;

	@Autowired
	public NegotiationDao negotiationDao;

	@Autowired
	public NegotiationAgreementDao negotiationAgreementDao;

	@Value("${spring.application.name}")
	private String applicationURL;


	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		Integer id = Integer.parseInt(moduleItemKey);
		Negotiations negotiations = negotiationAgreementDao.fetchNegotiationById(id);
		Map<String, String> placeHolder = getNegotiationsPlaceHolder(negotiations);
		return placeHolder;
	}

	private Map<String, String> getNegotiationsPlaceHolder(Negotiations negotiations) {
		Map<String, String> placeHolder = new HashedMap<>();
		String link = generateLinkToApplication(negotiations.getNegotiationId());
		placeHolder.put("{NEGOTIATION_ID}", negotiations.getNegotiationId() + "");
		placeHolder.put("{NEGOTIATOR_FULLNAME}", negotiations.getNegotiatorFullName() + "");
		placeHolder.put("{APPLICATION_URL}", link);
		if (negotiations.getEndDate() != null) {
			placeHolder.put("{END_DATE}", commonService.convertDateFormatBasedOnTimeZone(negotiations.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		} else {
			placeHolder.put("{END_DATE}", "");
		}
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return Constants.NEGOTIATION_MODULE_CODE.toString();
	}

	@Override
	public String getSubModuleCode() {
		return Constants.NEGOTIATION_SUBMODULE_CODE.toString();
	}

	public String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_NEGOTIATION_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

}

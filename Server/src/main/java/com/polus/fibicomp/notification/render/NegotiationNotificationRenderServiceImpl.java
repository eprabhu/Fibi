package com.polus.fibicomp.notification.render;

import java.util.Map;

import org.apache.commons.collections4.map.HashedMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.negotiation.pojo.Negotiations;

@Service(value = "negotiationNotificationRenderService")
@PropertySource("classpath:application.properties")
public class NegotiationNotificationRenderServiceImpl implements NegotiationNotificationRenderService{

	/**
	 * this method is used for get application base url.
	 */
	@Value("${spring.application.name}")
	private String applicationURL;
	
	@Autowired
	private CommonService commonService;
	
	/**
	 * this class file is used to to assign negotiation 
	 * related details to the notification engine
	 * these render service outputs are used to replace the 
	 * variable inside the {} email body saved on database
	 */
	@Override
	public Map<String, String> getNegotiationDefaultReplacementParameters(Negotiations negotiations) {
		Map<String, String> result = new HashedMap<>();
		result.put("{NEGOTIATION_ID}", negotiations.getNegotiationId() + "");
		result.put("{NEGOTIATOR_FULLNAME}", negotiations.getNegotiatorFullName() + "");
		result.put("{APPLICATION_URL}", applicationURL);
		if(negotiations.getEndDate()!=null)
		  result.put("{END_DATE}", commonService.convertDateFormatBasedOnTimeZone(negotiations.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		return result;
	}
}

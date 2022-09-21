package com.polus.fibicomp.notification.render;

import java.util.Map;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.negotiation.pojo.Negotiations;

@Service
public interface NegotiationNotificationRenderService {

	public Map<String, String> getNegotiationDefaultReplacementParameters(Negotiations negotiations);
	
}

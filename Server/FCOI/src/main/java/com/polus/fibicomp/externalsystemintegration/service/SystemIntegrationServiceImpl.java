package com.polus.fibicomp.externalsystemintegration.service;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Service(value = "systemIntegrationService")
public class SystemIntegrationServiceImpl implements SystemIntegrationService{

	@Value("${person.feed.api}")
	private String personFeedAPI;
	
	@Value("${project.task.api}")
	private String projectTsakAPI;
	
	@Value("${project.expenditure.api}")
	private String projectExpenditureAPI;
	
	@Value("${person.feed.api.token}")
	private String accessToken;
	
	@Value("${person.feed.api.token.type}")
	private String accessTokenType;
	
	@Override
	public String getExternalApiInfo() {
		
		String output = "[{\"person_feed_api\":\""+personFeedAPI+"\","				
						+ " \"project_task_api\":\""+personFeedAPI+"\","
						+ " \"project_expenditure_api\":\""+projectExpenditureAPI+"\","
						+ " \"token\":\""+accessToken+"\","
						+ " \"token_type\":\""+accessTokenType+"\"}]";			
		return output;
	}

}

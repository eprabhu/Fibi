package com.polus.integration.messageq;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.integration.dto.ProposalRequest;
import com.polus.integration.pojo.FibiCOIConnectDummy;
import com.polus.integration.service.IntegrationService;
import com.polus.integration.exception.service.MQRouterException;

@Configuration
public class FibiIntegrationListener {

	protected static Logger logger = LogManager.getLogger(FibiIntegrationListener.class.getName());

	@Autowired
	private IntegrationService integrationService;

	@RabbitListener(queues = "INTEGRATION_PROPOSAL_TRIAL_Q")
    public void receiveMessage(Message ampqMessage) {
		String message = new String(ampqMessage.getBody());
		logger.info("Message received from RabbitMQ: " + message);
        // Process the received message as required
		try {
			ProposalRequest proposalRequest = new ObjectMapper().readValue(message, ProposalRequest.class);
			FibiCOIConnectDummy response = integrationService.saveOrUpdateRecievedProposalDetail(proposalRequest);
			logger.info("Response: " + response.toString());
		} catch (Exception e) {
			throw new MQRouterException("ER004", e.getMessage(), e, null,
			          "INTEGRATION_PROPOSAL_TRIAL_Q", null,"FIBI.DIRECT.EXCHANGE", null,null, null,null);
		}
    }

}

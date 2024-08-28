package com.polus.integration.award.listener;

import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.integration.award.dto.AwardDTO;
import com.polus.integration.award.service.AwardIntegrationService;
import com.polus.integration.constant.Constant;
import com.polus.integration.dao.IntegrationDao;
import com.polus.integration.exception.service.MQRouterException;

import lombok.extern.slf4j.Slf4j;

@Configuration
@Slf4j
public class AwardIntegrationListener {

	@Autowired
	private AwardIntegrationService integrationService;

	@Autowired
	private IntegrationDao integrationDao;

	@Value("${fibi.messageq.queues.awardIntegration}")
	private String awardIntegrationQueue;

	@RabbitListener(queues = "${fibi.messageq.queues.awardIntegration}")
	public void feedAward(Message ampqMessage) {
		String response = new String(ampqMessage.getBody());
		log.info("Message received in feed award integration: {}", response);
		try {
			AwardDTO award = new ObjectMapper().readValue(response, AwardDTO.class);
			integrationService.feedAward(award);
		} catch (Exception e) {
			log.error("Exception in feed award integration: {}", response);
			throw new MQRouterException(Constant.ERROR_CODE, "Exception in feed award integration", e, e.getMessage(),
					awardIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,
					Constant.AWARD_MODULE_CODE, Constant.SUB_MODULE_CODE,
					Constant.AWARD_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
		}
	}

}

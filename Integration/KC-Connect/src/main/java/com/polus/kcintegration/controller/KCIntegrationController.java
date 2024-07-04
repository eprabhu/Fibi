package com.polus.kcintegration.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.reactive.function.client.WebClient;

import com.polus.kcintegration.dao.KCIntegrationDao;
import com.polus.kcintegration.dto.ProposalRequest;
import com.polus.kcintegration.dto.FibiCOIConnectDummyDTO;

import reactor.core.publisher.Mono;

@RestController
public class KCIntegrationController {

	protected static Logger logger = LogManager.getLogger(KCIntegrationController.class.getName());

	@Autowired
	private RabbitTemplate rabbitTemplate;

	@Autowired
	private KCIntegrationDao kcIntegrationDao;

	@Autowired
	private WebClient webClient;

	@Value("${fibiIntegrationClient.url}")
	private String fibiIntegrationClient;

	@PostMapping("/recieveProposalDetails")
	public void recieveProposalDetails(@RequestBody ProposalRequest proposalRequest) {
		logger.info("Request for receive proposal details");
		logger.info("Inside kc connect {}");
		logger.info("CoiProjectTypeCode {}", proposalRequest.getCoiProjectTypeCode());
		logger.info("HomeUnit {}", proposalRequest.getHomeUnit());
		logger.info("ModuleItemKey {}", proposalRequest.getModuleItemKey());
		logger.info("PersonId {}", proposalRequest.getPersonId());
		try {
			rabbitTemplate.convertAndSend("FIBI.DIRECT.EXCHANGE","INTEGRATION_PROPOSAL_TRIAL_Q", new Message(kcIntegrationDao.convertObjectToJSON(proposalRequest).getBytes()));
		} catch (Exception e) {
			logger.error("Unexpected error occurred: {}", e.getMessage());
			e.printStackTrace();
		}
	}

	@PostMapping("/recieveProposalDetailsUsingWC")
	public void recieveProposalDetailsUsingWC(@RequestBody ProposalRequest proposalRequest) {
		logger.info("Request for receive proposal details");
		logger.info("Inside kc connect {}");
		Mono<FibiCOIConnectDummyDTO> response = webClient.post().uri(fibiIntegrationClient + "/saveProposalDetails")
				.body(Mono.just(proposalRequest), ProposalRequest.class).retrieve().bodyToMono(FibiCOIConnectDummyDTO.class);
		response.subscribe(responseObj -> {
			logger.info("Success {}", responseObj);
			processProposalResponse(responseObj);
		}, error -> {
			logger.error("Error during WebClient call in KC connect application", error);
		});
	}
	
	private void processProposalResponse(FibiCOIConnectDummyDTO response) {
        logger.info("Parsed ProposalResponse - DummyId: {}, TypeCode: {}, ProposalId: {}, PersonId: {}, UnitNumber: {}",
                response.getDummyId(), response.getTypeCode(), response.getProposalId(), response.getPersonId(), response.getUnitNumber());
    }

}

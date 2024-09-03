package com.polus.integration.feedentity.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;

import com.polus.integration.feedentity.client.KCFeignClient;
import com.polus.integration.feedentity.dao.EntityIntegrationDao;
import com.polus.integration.feedentity.dto.EntityDTO;
import com.polus.integration.feedentity.dto.EntityResponse;
import com.polus.integration.repository.UserTokensRepository;

import feign.FeignException;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class EntityOutboundIntegrationService {

	@Autowired
	private EntityIntegrationDao entityIntegrationDao;

	@Autowired
	private KCFeignClient kcFeignClient;

	@Autowired
	private UserTokensRepository userTokensRepository;

	@Value("${kc.integration.user.name}")
	private String userName;

	public void getEntityDetails(Integer entityId, String personId) {
		log.info("Requesting feedEntityDetails for entityId: {} and personId: {}", entityId, personId);

		try {
			if (personId == null) {
				personId = userTokensRepository.findByUserName(userName).getPersonId();
			}

			List<EntityDTO> entityDTOs = entityIntegrationDao.getEntityDetails(entityId);
			if (entityDTOs.isEmpty()) {
				log.warn("No entity details found for entityId: {}", entityId);
				return;
			}

			for (EntityDTO entityDTO : entityDTOs) {
				entityDTO.setUpdatedBy(personId);
				entityDTO.setCreatedBy(personId);
				entityDTO.setIsCreateSponsor(Boolean.TRUE);
				entityDTO.setIsCreateOrganization(Boolean.TRUE);

				try {
					EntityResponse entityResponse = kcFeignClient.feedEntityDetails(entityDTO);
					entityIntegrationDao.updateEntitySponsorInfoByParams(entityResponse);
				} catch (FeignException | DataAccessException e) {
					log.error("Error processing entityId: {}, personId: {}: {}", entityId, personId, e.getMessage(), e);
				} catch (Exception e) {
					log.error("Unexpected error for entityId: {}, personId: {}: {}", entityId, personId, e.getMessage(), e);
				}
			}
		} catch (DataAccessException e) {
			log.error("Database error while retrieving entity details for entityId: {}: {}", entityId, e.getMessage(), e);
		} catch (Exception e) {
			log.error("Unexpected error while retrieving entity details for entityId: {}: {}", entityId, e.getMessage(), e);
		}
	}

}

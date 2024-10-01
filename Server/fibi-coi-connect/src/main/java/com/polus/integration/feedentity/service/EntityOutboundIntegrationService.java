package com.polus.integration.feedentity.service;

import java.util.List;
import java.util.regex.Pattern;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;

import com.polus.integration.feedentity.client.KCFeignClient;
import com.polus.integration.feedentity.dao.EntityIntegrationDao;
import com.polus.integration.feedentity.dto.EntityDTO;
import com.polus.integration.feedentity.dto.EntityResponse;

import feign.FeignException;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class EntityOutboundIntegrationService {

	@Autowired
	private EntityIntegrationDao entityIntegrationDao;

	@Autowired
	private KCFeignClient kcFeignClient;

	@Value("${kc.integration.user.name}")
	private String userName;

	public void getEntityDetails(Integer entityId) {
		log.info("Requesting feedEntityDetails for entityId: {}", entityId);
		String yearPattern = "\\d{4}";
		String monthYearPattern = "\\d{4}-\\d{2}";

		try {
			List<EntityDTO> entityDTOs = entityIntegrationDao.getEntityDetails(entityId);
			if (entityDTOs.isEmpty()) {
				log.warn("No entity details found for entityId: {}", entityId);
				return;
			}

			for (EntityDTO entityDTO : entityDTOs) {
				String incorporationDate = entityDTO.getIncorporationDate();
				log.info("incorporationDate : {}", incorporationDate);

				if (incorporationDate != null && !incorporationDate.isEmpty()) {
					if (Pattern.matches(yearPattern, incorporationDate)) {
						incorporationDate = incorporationDate + "-01-01";
					} else if (Pattern.matches(monthYearPattern, incorporationDate)) {
						incorporationDate = incorporationDate + "-01";
					} else {
						log.warn("Invalid incorporation date format: {}", incorporationDate);
					}
					log.info("updated incorporationDate : {}", incorporationDate);
					entityDTO.setIncorporationDate(incorporationDate);
				}

				try {
					EntityResponse entityResponse = kcFeignClient.feedEntityDetails(entityDTO);
					entityIntegrationDao.updateEntitySponsorInfoByParams(entityResponse);
				} catch (FeignException | DataAccessException e) {
					log.error("Error processing entityId: {} : {}", entityId, e.getMessage(), e);
				} catch (Exception e) {
					log.error("Unexpected error for entityId: {} : {}", entityId, e.getMessage(), e);
				}
			}
		} catch (DataAccessException e) {
			log.error("Database error while retrieving entity details for entityId: {}: {}", entityId, e.getMessage(), e);
		} catch (Exception e) {
			log.error("Unexpected error while retrieving entity details for entityId: {}: {}", entityId, e.getMessage(), e);
		}
	}

}

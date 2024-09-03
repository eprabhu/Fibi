package com.polus.kcintegration.entity.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.kcintegration.entity.dao.EntityKCIntegrationDao;
import com.polus.kcintegration.entity.dto.EntityDTO;
import com.polus.kcintegration.entity.dto.EntityResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class EntityKCIntegrationService {

	@Autowired
	private EntityKCIntegrationDao kcIntegrationDao;

	public EntityResponse feedEntityDetails(EntityDTO entityDTO) {
		log.info("Requesting for feedEntityDetails!");
		log.info("entityId : {}", entityDTO.getEntityId());
		return kcIntegrationDao.feedEntityDetailsToSponsorAndOrg(entityDTO);
	}


}

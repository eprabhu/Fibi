package com.polus.fibicomp.globalentity.dao;

import java.util.Map;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.pojo.Entity;

@Transactional
@Service
public interface EntityDetailsDAO {

	public int createEntity(Entity entity);

	public void updateEntity(EntityRequestDTO dto);

	public Entity fetchEntityDetails(Integer entityId);

	public Map<String, Object> getEntityTabStatus(Integer entityId);

}

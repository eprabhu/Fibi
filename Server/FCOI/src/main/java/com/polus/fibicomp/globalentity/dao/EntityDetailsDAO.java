package com.polus.fibicomp.globalentity.dao;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.pojo.GlobalEntity;

@Transactional
@Service
public interface EntityDetailsDAO {

	public int createEntity(GlobalEntity entity);

	public void updateEntity(EntityRequestDTO dto);

	public GlobalEntity fetchEntityDetails(Integer entityId);

}

package com.polus.fibicomp.globalentity.dao;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.globalentity.dto.SponsorRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntitySponsorInfo;

@Transactional
@Service
public interface SponsorDAO {

	Integer saveDetails(EntitySponsorInfo entity);

	void updateDetails(SponsorRequestDTO dto);

}

package com.polus.fibicomp.medusa.service;

import com.polus.fibicomp.medusa.dto.Medusa;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.medusa.dto.MedusaDTO;

@Service
public interface MedusaService {

	/**
	 * this method is used for get medusa details
	 * @param vo
	 * @return
	 */
	public MedusaDTO getMedusa(Integer moduleCode, String ProjectId);
	
	/**
	 * this method is used for get medusa more details of IP,Award,PD and Negotiation
	 * @param vo
	 * @return
	 */
	public String getMedusaMoreDetail(MedusaDTO vo);

	/**
	 * this method is used to get service requests of a module
	 * @param medusa
	 * @return
	 */
	public String getModuleServiceRequestDetail(Medusa medusa);
}

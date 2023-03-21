package com.polus.fibicomp.medusa.dao;

import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.medusa.dto.MedusaDTO;

@Service
public interface MedusaDao {

	/**
	 * this method is used for get medusa hierarchy details
	 * @param vo
	 * @return
	 */
	public MedusaDTO getMedusa(String projectId, Integer moduleCode);

	/**
	 * this method is used for get medusa more details of IP,Award,PD and Negotiation
	 * @param vo
	 * @return
	 */
	public HashMap<String, Object> getProjectDetailsSP(String moduleKey, String moduleNumber);

	/**
	 * this method is used for get medusa service request details of a module
	 *
	 * @param vo
	 * @return
	 */
	public ArrayList getServiceRequestDetail(int moduleCode, String moduleItemKey,int subModuleCode, String subModuleItemKey, String personId);
}

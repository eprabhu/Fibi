package com.polus.fibicomp.grantcall.dao;

import java.util.List;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPICriteria;
import com.polus.fibicomp.grantcall.pojo.KPIType;

@Transactional
@Service
public interface GrantCallKPIDao {

	/**
	 * This method is used to fetch all KPIs .
	 * 
	 * @return A list of KPIs.
	 */
	public List<KPIType> fetchAllKPIs();

	/**
	 * This method used to fetch KPIByGrantCall by id
	 * 
	 * @param grantCallId
	 * @return details of grant call KPIs
	 */
	public List<GrantCallKPI> fetchKPIByGrantCallId(Integer grantCallId);

	/**
	 * This method is used to save and update grantcall KPIs
	 * 
	 * @param GrantCallKPI - grantCallKpi
	 */
	public GrantCallKPI saveOrUpdateGrantCallKPI(GrantCallKPI grantCallKpi);

	/**
	 * This method is used to delete GrantCallKPI based on id.
	 * 
	 * @param grantCallKpiId         - Id of the GrantCallKPI.
	 * @param grantCallKpiCriteriaId - Id of the GrantCallKPICriteria.
	 * @param grantCallId            - Id of the GrantCall.
	 * @return success message.
	 */
	public String deleteGrantCallKPI(Integer grantCallId, Integer grandCallKpiId, Integer grantCallKpiCriteriaId);

	/**
	 * This method is used to fetch Grant Call KPI Criteria based on grantCallKPI.
	 * 
	 * @param grantCallKpiId - type code of grant call KPI.
	 * @return A list of GrantCallKPICriteria corresponding to the type.
	 */
	public List<GrantCallKPICriteria> fetchgrantCallKPICriteria(Integer grantCallKpiId);

	/**
	 * This method is used to delete a GrantCallKPI.
	 * @param grantKPIId - GrantCallKPI of GrantCall
	 * @return
	 */
	public GrantCallKPI deleteGrantCallKPI(GrantCallKPI grantCallKPI);

}

package com.polus.fibicomp.grantcall.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.vo.GrantCallKPIVO;

@Transactional
@Service
public interface GrantCallKPIService {

	/**
	 * This method is used to fetch the details of getKPIByGrantCall by id
	 * 
	 * @param vo
	 * @return object of GrantCallKPIVO as string
	 */
	public String getKPIByGrantCall(GrantCallKPIVO vo);

	/**
	 * This method is used to save or update grant call KPIs.
	 * 
	 * @param vo - Object of GrantCallKPIVO class.
	 * @return set of values to figure out details about a grant call KPIs.
	 */
	public String saveUpdateGrantCallKPI(GrantCallKPIVO vo);

	/**
	 * This method is used to delete GrantCallKPI based on id.
	 * 
	 * @param vo - Object of GrantCallKPIVO class.
	 * @return success message.
	 */
	public String deleteGrantCallKPI(GrantCallKPIVO vo);

}

package com.polus.fibicomp.person.delegation.delegationService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.person.delegation.vo.DelegationVO;

@Transactional
@Service
public interface DelegationService {

	/**
	 * This method is used save or update delegation details
	 * @param vo
	 * @return.
	 */
	public String saveOrUpdateDeligation(DelegationVO vo);

	/**
	 * This method is used get delegation by params
	 * @param personId
	 * @return.
	 */
	public String loadDelegationByPersonId(String personId);

	/**
	 * This method is used get update delegation details
	 * @param vo
	 * @return.
	 */
	public String updateDeligationStatus(DelegationVO vo);

}

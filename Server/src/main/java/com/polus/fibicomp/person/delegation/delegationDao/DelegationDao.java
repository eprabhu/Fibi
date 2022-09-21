package com.polus.fibicomp.person.delegation.delegationDao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.person.delegation.pojo.DelegationStatus;
import com.polus.fibicomp.person.delegation.pojo.Delegations;

@Transactional
@Service
public interface DelegationDao {

	/**
	 * This method is used save or update delegation details
	 * @param delegation
	 * @return delegation.
	 */
	public Delegations saveOrUpdateDelegation(Delegations delegation);

	/**
	 * This method is used get delegations details 
	 * @param personId
	 * @return list of delegations.
	 */
	public List<Delegations> getDelegationByPersonId(String personId);

	/**
	 * This method is used get delegation by delegation id
	 * @param delegationId
	 * @return delegation.
	 */
	public Delegations getDelegationByDelegationId(Integer delegationId);

	/**
	 * This method is used get delegation by params
	 * @param personId
	 * @return delegations.
	 */
	public Delegations loadDelegationByParams(String personId, List<String> delegationStatusCode);

	/**
	 * This method is used get delegation status by params
	 * @param delegationStatusCode.
	 * @return delegation status
	 */
	public DelegationStatus loadDelegationStatusById(String delegationStatusCode);

}

package com.polus.fibicomp.scopusintegration.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.scopusintegration.pojo.AwardScopus;
import com.polus.fibicomp.scopusintegration.pojo.Scopus;

@Service
public interface ScopusDao {

	/**
	 * This method is used to save a Scopus and its details.
	 * 
	 * @param scopus
	 * @return it returns Success message.
	 */
	public void saveOrUpdateScopusInfo(Scopus scopus);

	/**
	 * This method is used to fetch filtered Scopus based on input string.
	 * 
	 * @param searchString - input string.
	 * @return a list of Scopus.
	 */
	public List<Scopus> findScopus(String searchString);

	/**
	 * This method is used to save the AwardScopus.
	 * 
	 * @param awardScopus - awardScopus.
	 * @return an object of AwardScopus.
	 */
	public AwardScopus saveOrUpdateAwardScopus(AwardScopus awardScopus);

	/**
	 * This method is used to Award Scopus based on awardScopusId.
	 * 
	 * @param awardScopusId - awardScopusId.
	 * @return an object of AwardScopus.
	 */
	public AwardScopus getAwardScopusBasedOnId(Integer awardScopusId);

	/**
	 * This method is used to deleteAwardScopus.
	 * 
	 * @param awardScopus - awardScopus object.
	 * @return an object of AwardScopus.
	 */
	public void deleteAwardScopus(AwardScopus awardScopus);

	/**
	 * This method is used to fetchAllAwardScopus.
	 * 
	 * @param awardId - awardId.
	 * @return an List of AwardScopus.
	 */
	public List<AwardScopus> fetchAllAwardScopus(Integer awardId);

	public String getConfigurationValue(String scopusApi);

}

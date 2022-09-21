package com.polus.fibicomp.claims.claimsIntegration.ics.dao;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.ics.pojo.IcsStudentTravelDetail;
import com.polus.fibicomp.pojo.Country;

@Transactional
@Service
public interface IcsDao {

	/**
	 * This method is used to save or update claim student travel details
	 * @param claimStudentTravelDetail
	 * @return object of IcsStudentTravelDetail
	 */
	public IcsStudentTravelDetail saveOrUpdateClaimStudentTravelDetail(IcsStudentTravelDetail claimStudentTravelDetail);

	/**
	 * This method is used to get CountryCode based on CountryName
	 * @param countryName
	 * @return object of Country
	 */
	public Country fetchCountryCodeByCountryName(String countryName);

}

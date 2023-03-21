package com.polus.fibicomp.scopusintegration.service;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.scopusintegration.vo.ScopusVO;

@Service
public interface ScopusService {

	/**
	 * This method is used to fetch Scopus Feed data.
	 * 
	 * @param
	 * @return A list of Scopus and its details.
	 */
	public void fetchScopusAPIResponse();

	/**
	 * This method is used to fetch Scopus.
	 * 
	 * @param searchString - searchString
	 * @return A list of details of Scopus.
	 */
	public String findScopus(String searchString);

	/**
	 * This method is used to save Award Scopus.
	 * 
	 * @param vo - awardVO object
	 * @return A list of details saved Scopus.
	 */
	public String saveAwardScopus(ScopusVO vo);

	/**
	 * This method is used to delete Award Scopus.
	 * 
	 * @param vo - awardVO object
	 * @return A String response.
	 */
	public String deleteAwardScopus(ScopusVO vo);

	/**
	 * This method is used to load All Award Scopus.
	 * 
	 * @param vo - ScopusVO object
	 * @return A String of details saved ScopusVO.
	 */
	public String loadAllAwardScopus(ScopusVO vo);
	/**
	 * This method is used to get Scopus configuration.
	 * 
	 * @param 
	 * @return 
	 */
	public void getScopusConfurationData();
	
	

}

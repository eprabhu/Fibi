package com.polus.fibicomp.rolodex.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.rolodex.vo.RolodexSearchResult;
import com.polus.fibicomp.rolodex.vo.RolodexVO;

@Transactional
@Service
public interface RolodexService {

	/**
	 * This method is used to search the rolodex details
	 * @param searchString
	 * @return list of rolodex details based on search string
	 */
	public List<RolodexSearchResult> findRolodex(String searchString);

	/**
	 * This method is used to get rolodex details by id
	 * @param vo
	 * @return object with rolodex details
	 */
	public String getRolodexDetailById(RolodexVO vo);

	/**
	 * This method is used to save and update rolodex details
	 * @param vo
	 * @return object with success message and rolodex object
	 */
	public String saveOrUpdateRolodex(RolodexVO vo);

	/**
	 * This method is used get rolodex details
	 * @param vo
	 * @return object with success message and rolodex object
	 */
	public String getAllRolodexes(RolodexVO vo);

	/**
	 * @param searchString is the search keyword
	 * @return a list of country matching the search keyword
	 */
	public List<Country> findCountryList(String searchString);

}

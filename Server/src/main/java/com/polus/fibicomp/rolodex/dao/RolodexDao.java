package com.polus.fibicomp.rolodex.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.rolodex.vo.RolodexSearchResult;
import com.polus.fibicomp.rolodex.vo.RolodexVO;

@Transactional
@Service
public interface RolodexDao {

	/**
	 * This method is used to get the rolodex details by search
	 * @param searchString
	 * @return list of object with fullname and rolodex id
	 */
	public List<RolodexSearchResult> findRolodex(String searchString);

	/**
	 * This method is used to get rolodex details by id
	 * @param rolodexId
	 * @return object with details of corresponding object
	 */
	public Rolodex getRolodexDetailById(Integer rolodexId);

	/**
	 * This method is used to save and update rolodex details
	 * @param rolodex
	 * @return object with details of corresponding object
	 */
	public Rolodex saveOrUpdateRolodex(Rolodex rolodex);

	/**
	 * This method is used to delete  details
	 * @param rolodex
	 * @return object with details of corresponding object
	 */
	public void deleteRolodex(Rolodex rolodex);

	/**
	 * This method is used to get rolodex details
	 * @param Vo
	 * @return object with details of corresponding object
	 */
	public RolodexVO getAllRolodexes(RolodexVO vo);

	/**
	 * This method is used to check emailid already exists or not by emailId
	 * @param emailId
	 * @return boolean value of parameter.
	 */
	public boolean checkUniqueEmailAddress(String emailId);

	/**
	 * This method is used to check the emailid of the existing user
	 * @param emailId, rolodexId
	 * @return boolean value of parameter.
	 */
	public boolean checkPersonEmailAddress(Integer rolodexId, String emailAddress);

	/**
	 * This method is used to save and update organization details
	 * @param organization
	 * @return object with details of Organization object
	 */
	public Organization saveOrUpdateOrganization(Organization organization);

	/**
	 * This method is used to get country detail by countryCode
	 * @param countryCode
	 * @return country object
	 */
	public Country fetchCountryByCountryCode(String countryCode);

}

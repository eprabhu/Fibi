package com.polus.fibicomp.general.service;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.SponsorMaintenanceVO;

@Service
public interface GeneralInformationService {

	/**
	 * This method is used to fetch all sponsor information.
	 * @param String - Sponsor Code.
	 * @return SponsorMaintenanceVO All sponsor object.
	 */
	public SponsorMaintenanceVO fetchSponsorData(String sponsorCode);

	/**
	 * This method will initialize object for creating a new sponsor.
	 * @return SponsorMaintenanceVO All sponsor object.
	 */
	public SponsorMaintenanceVO createNewSponsor();

	/**
	 * This method will add/delete/update a sponsor to the database
	 * @param SponsorMaintenanceVO
	 * @return SponsorMaintenanceVO All sponsor object.
	 */
	public SponsorMaintenanceVO saveSponsor(SponsorMaintenanceVO sponsorMaintenanceVO);

	/**
	 * this method is used to to synchronize person right table after role type
	 * @return
	 */
	public String syncPersonRole();

	public String getAllSponsors(CommonVO vo);

	public String fetchHelpText(CommonVO vo);
	
	/**
	 * module configuration or section configuration if sectionCode is not null
	 * @param sectionCode
	 * @return module configuration
	 */
	public String getModulesConfiguration(String sectionCode);

}

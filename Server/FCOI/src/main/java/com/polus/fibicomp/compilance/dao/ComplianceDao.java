package com.polus.fibicomp.compilance.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.compilance.pojo.AcProtocol;
import com.polus.fibicomp.compilance.pojo.AcProtocolStatus;
import com.polus.fibicomp.compilance.pojo.IrbProtocol;
import com.polus.fibicomp.compilance.pojo.IrbProtocolStatus;
import com.polus.fibicomp.compilance.pojo.SpecialReviewUsage;
import com.polus.fibicomp.compilance.vo.ProtocolVO;
import com.polus.fibicomp.pojo.SpecialReviewType;

@Transactional
@Service
public interface ComplianceDao {

	/**
	 * This method is used to fetch all special review types.
	 * @return A list of SpecialReviewType.
	 */
	public List<SpecialReviewType> fetchAllSpecialReviewType();

	/**
	 * This method is used to fetch SpecialReview Usage By ModuleCode.
	 * @param moduleCode
	 * @return A list of SpecialReviewUsage.
	 */
	public List<SpecialReviewUsage> fetchSpecialReviewUsageByModuleCode(String moduleCode);

	/**
	 * This method is used to load Irb Protocol Details.
	 * @param ProtocolVO - vo
	 * @return A list of IrbProtocol.
	 */
	public List<IrbProtocol> loadIrbProtocolDetail(ProtocolVO vo);

	/**
	 * This method is used to load Ac Protocol Details.
	 * @param ProtocolVO - vo
	 * @return A list of AcProtocol.
	 */
	public List<AcProtocol> loadAcProtocolDetail(ProtocolVO vo);

	/**
	 * This method is used to fetch Latest Irb Prtocol Details.
	 * @param protocolNumber
	 * @return
	 */
	public IrbProtocol fetchLatestIrbPrtocol(String protocolNumber);

	/**
	 * This method is used to fetch Latest Ac Prtocol Details.
	 * @param protocolNumber
	 * @return
	 */
	public AcProtocol fetchLatestAcPrtocol(String protocolNumber);

	/**
	 * This method is used to fetch Latest Ac Prtocol status Details.
	 * @return list of AcProtocolStatus
	 */
	public List<AcProtocolStatus> getAcProtocolStatus();

	/**
	 * This method is used to fetch Latest Irb Prtocol status Details.
	 * @return list of IrbProtocolStatus
	 */
	public List<IrbProtocolStatus> getIrbProtocolStatus();

}

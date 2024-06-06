package com.polus.formbuilder.customdataelement.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.formbuilder.customdataelement.VO.CustomDataElementVO;

@Transactional
@Service
public interface CustomDataElementService {

	/**
	 * This method is used to create, update the CustomDataElements
	 * @param vo
	 * @return object of CustomelementVO as string
	 */
	public ResponseEntity<Object> configureCustomElement(CustomDataElementVO  vo);

	/**
	 * This method is used to fetch all the CustomDataElements
	 * @param vo
	 * @return object of CustomelementVO as string
	 */
	public ResponseEntity<Object> fetchAllCustomElement(CustomDataElementVO vo);

	/**
	 * This method is used to fetch the details of CustomDataElement by id
	 * @param vo
	 * @return object of CustomelementVO as string
	 */
	public CustomDataElementVO fetchCustomElementById(CustomDataElementVO vo);

	/**
	 * This method is used to save the answers of custom elements
	 * @param vo
	 * @return saved response
	 */
	public CustomDataElementVO saveCustomResponse(CustomDataElementVO vo);

	/**
	 * This method is used get the system lookups
	 * @param vo
	 * @return lookup based on custom types
	 */
	public ResponseEntity<Object> getSystemLookupByCustomType(CustomDataElementVO vo);

}

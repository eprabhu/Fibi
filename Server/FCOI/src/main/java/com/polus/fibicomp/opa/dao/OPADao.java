package com.polus.fibicomp.opa.dao;

import org.springframework.stereotype.Service;

@Service
public interface OPADao {

	boolean isOpaDisclosureRequired(String personId);

	Integer createOpaDisclosure(String personId, String homeUnit);

}

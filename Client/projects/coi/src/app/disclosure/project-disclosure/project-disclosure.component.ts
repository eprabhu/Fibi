import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { ElasticConfigService } from 'projects/fibi/src/app/common/services/elastic-config.service';
import { Subscription } from 'rxjs';
import { getEndPointOptionsForDepartment, getEndPointOptionsForProposalDisclosure, getEndPointOptionsForSponsor } from '../../../../../fibi/src/app/common/services/end-point.config';
import { openModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CoiService } from '../services/coi.service';
import { DataStoreService } from '../services/data-store.service';

declare var $: any;
@Component({
  selector: 'app-project-disclosure',
  templateUrl: './project-disclosure.component.html',
  styleUrls: ['./project-disclosure.component.scss']
})
export class ProjectDisclosureComponent implements OnInit {

  isShowResultCard = false;
  clearProposalField: String;
  clearFieldSponsorField: String;
  clearPIField: String;
  clearLUField: String;
  proposalDetails: any = {};
  projectDisclosureValidation = new Map();
  ProposalSearchOptions: any;
  isSearchExternalProposal = false;
  $subscriptions: Subscription[] = [];
  piElasticSearchOptions: any = {};
  unitHttpOptions: any = {};
  sponsorSearchOptions: any = {};
  
  constructor(public coiService: CoiService, public _dataStore: DataStoreService,
              private _router: Router,
              private _elasticConfig: ElasticConfigService) { }

  ngOnInit() {
    openModal('createProjectDisclosureModal');
    this.ProposalSearchOptions = getEndPointOptionsForProposalDisclosure();
    this.piElasticSearchOptions = this._elasticConfig.getElasticForPerson();
    this.unitHttpOptions = getEndPointOptionsForDepartment();
    this.sponsorSearchOptions = getEndPointOptionsForSponsor();

  }

  clearDisclosureModal() {
		this.isShowResultCard =  false;
		this.clearProposalField = new String('true');
		this.clearFieldSponsorField = new String('true');
		this.clearPIField = new String('true');
		this.clearLUField = new String('true');
		this.proposalDetails = {};
		this.proposalDetails.moduleItemId = null;
		this.proposalDetails.title = null;
    this.projectDisclosureValidation.clear();
    }

    selectedProposal(event) {
      if (event) {
        this.proposalDetails = event;
        this.proposalDetails.proposalId = event.moduleItemId;
        this.isShowResultCard =  true;
      } else {
        this.clearSearchFields();
      }
      }

      clearSearchFields() {
        this.clearProposalField = new String('true');
        this.clearFieldSponsorField = new String('true');
        this.clearPIField = new String('true');
        this.clearLUField = new String('true');
        this.isShowResultCard = false;
        this.proposalDetails = {};
        this.ProposalSearchOptions = getEndPointOptionsForProposalDisclosure();
        this.projectDisclosureValidation.clear();
      }
    
      createProjectDisclosureAPI() {
        if (this.validateProject()) {
            this.$subscriptions.push(this.coiService.createDisclosure({
                moduleItemId: this.proposalDetails.moduleItemId,
                moduleCode: this._dataStore.currentDashboardTab === 'PROPOSAL_DISCLOSURES' ? 3 : '',
                disclosureCategoryType: this._dataStore.currentDashboardTab
            }).subscribe((data: any) => {
                if (data) {
                    $('#createProposalDisclosureModal').modal('hide');
                    this._router.navigate(['/fibi/coi/screening-questionnaire'], {
                        queryParams: {
                            disclosureId: data.coiDisclosure.disclosureId
                        }
                    });
                }
            }, _err => {
            }));
        }
    }

    validateProject() {
      this.projectDisclosureValidation.clear();
      if (!this.proposalDetails || !this.proposalDetails.moduleItemId) {
          this.projectDisclosureValidation.set('proposalSearch', 'Please select a proposal to create disclosure.');
      }
      return this.projectDisclosureValidation.size === 0 ? true : false;
  }


}

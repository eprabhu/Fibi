import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { ActivityService } from '../../../../../fibi/src/app/agreement/agreement-shared/activity-track/activity.service';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { environment } from '../../../environments/environment';
import { CommonService } from '../../common/services/common.service';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../../fibi/src/app/common/services/end-point.config';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../../../fibi/src/app/common/utilities/date-utilities';
import { DEFAULT_DATE_FORMAT } from 'projects/fibi/src/app/app-constants';

export interface EndpointOptions {
  contextField: string;
  formatString: string;
  path: string;
  defaultValue: string;
  params: string;
}
@Component({
  selector: 'app-add-sfi',
  templateUrl: './add-sfi.component.html',
  styleUrls: ['./add-sfi.component.scss'],
  providers: [ActivityService],
  animations: [slideHorizontal]
})
export class AddSfiComponent implements OnInit {

  isSaving = false;
  scrollHeight: number;
  coiEntity: any = {
    entityTypeCode: null
  };
  additionalDetails: any = {
    sponsorsResearch: false
  };
  deployMap = environment.deployUrl;
  @ViewChild('sfiNavOverlay', { static: true }) sfiNavOverlay: ElementRef;
  isAddAttachment = false;
  isAddAssignee = false;
  dateTime: string;
  datePlaceHolder = DEFAULT_DATE_FORMAT;
  isReadMore: false;
  showRelationshipModal = false;
  clearField: any = false;
  EntitySearchOptions: any = {};
  countrySearchOptions: EndpointOptions;
  clearCountryField: any;
  $subscriptions: Subscription[] = [];
  mandatoryList = new Map();
  emailWarningMsg: any;
  sfiLookUpList: any = {};
  isExpandedAdditionalDetails = true;
  isResultFromSearch = false;

  constructor(public sfiService: SfiService, public _commonService: CommonService,
    private _activatedRoute: ActivatedRoute, private _router: Router) { }

  ngOnInit() {
    // this.sfiLookUpList = this.sfiService.lookups;
    this.getSFILookup();
    this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
      this.coiEntity.entityId = params['entityId'];
      if (this.coiEntity.entityId) {
        this.getSfiDetails();
      }
    }));
    this.showSfiNavBar();
    this.EntitySearchOptions = getEndPointOptionsForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry();
  }

  getSFILookup() {
    this.$subscriptions.push(this.sfiService.addSFILookUp().subscribe((res: any) => {
      this.sfiLookUpList = res;
    }));
  }

  getSfiDetails() {
    this.$subscriptions.push(this.sfiService.getSFIDetails(this.coiEntity.entityId).subscribe(data => {

    }));
  }

  removeEntityId() {
    this._router.navigate([], {
      queryParams: { entityId: null },
      queryParamsHandling: 'merge'
    })
  }

  setEntityTypeObj() {
    this.coiEntity.entityType = this.sfiLookUpList.entityType.find(ele =>
      this.coiEntity.entityTypeCode == ele.entityTypeCode);
  }

  hideSfiNavBar() {
    if (!this.isSaving) {
      this.sfiService.isShowSfiNavBar = false;
      this.showSfiNavBar();
      this.removeEntityId();
    }
  }

  showSfiNavBar() {
    if (this.sfiService.isShowSfiNavBar) {
      this.sfiNavOverlay.nativeElement.style.display = 'block';
      this.scrollHeight = document.documentElement.scrollTop;
      document.body.style.overflow = 'hidden';
      document.documentElement.style.top = - this.scrollHeight + 'px';
    } else {
      this.sfiNavOverlay.nativeElement.style.display = 'none';
      document.body.style.overflow = 'auto';
      document.documentElement.scrollTop = this.scrollHeight;
    }
  }

  addEntityToggle(event) {
    hideModal(event);
  }

  hideRelationshipModal(event) {
    this.showRelationshipModal = event;
    this.showSfiNavBar();
  }

  //  createSFI() {
  //     if (this.checkMandatoryFilled() && !this.isSaving) {
  //       this.isSaving = true;
  //       // this.entityDetails.involvementStartDate = parseDateWithoutTimestamp(this.entityDetails.involvementStartDate);
  //       // this.entityDetails.involvementEndDate = parseDateWithoutTimestamp(this.entityDetails.involvementEndDate);
  //       this.$subscriptions.push(this.sfiService.createSFI({ 'coiFinancialEntity': this.entityDetails,
  //             disclosureId: parseInt(this._activatedRoute.snapshot.queryParamMap.get('dId')),
  //             proposalDisclosureWithNoSfi : this._activatedRoute.snapshot.queryParamMap.get('isSFINotAvailable') == 'true' ? true : false
  //       })
  //         .subscribe((data: any) => {
  //           this.sfiService.sfiDetails = JSON.parse(JSON.stringify(data));
  //           this.isSaving = false;
  //           this.showRelationshipModal= true;
  //           this._router.navigate([], {
  //             queryParams: {
  //               entityId: data.coiFinancialEntity.coiFinancialEntityId
  //             },
  //             queryParamsHandling: 'merge',
  //           });
  //           this.entityDetails = data.coiFinancialEntity;
  //           this.EntitySearchOptions.defaultValue = this.coiEntity.entityName;
  //           this.entityDetails.involvementStartDate = getDateObjectFromTimeStamp(this.entityDetails.involvementStartDate);
  //           this.entityDetails.involvementEndDate = getDateObjectFromTimeStamp(this.entityDetails.involvementEndDate);

  //         },
  //           err => {
  //             this.isSaving = false;
  //           }));
  //     }
  //   }

  createSFI() {
    if (!this.checkMandatoryFilled() && !this.isSaving) {
      return null;
    }
    this.coiEntity.entityId ? this.saveAdditionalDetails() : this.saveEntityDetails()
  }

  saveEntityDetails() {
    this.$subscriptions.push(this.sfiService.saveOrUpdateCoiEntity({ coiEntity: this.coiEntity }).subscribe((data: any) => {
      this.coiEntity = data.coiEntity;
      this.saveAdditionalDetails();
    }));
  }

  saveAdditionalDetails() {
    this.$subscriptions.push(this.sfiService.createSFI(
      {
        personEntity: {
          entityId: this.coiEntity.entityId,
          entityNumber: this.coiEntity.entityNumber,
          ...this.additionalDetails
        }
    }).subscribe((data: any) => {
      this.additionalDetails = data.personEntity;
      this.isSaving = false;
      this.showRelationshipModal= true;
      this.additionalDetails.involvementStartDate = getDateObjectFromTimeStamp(this.additionalDetails.involvementStartDate);
      this.additionalDetails.involvementEndDate = getDateObjectFromTimeStamp(this.additionalDetails.involvementEndDate);
    }));
  }

  selectNewEntity(event) {
    this.coiEntity.entityName = event.searchString;
  }

  selectedEvent(event) {
    if (event) {
      this.isResultFromSearch = true;
      this.coiEntity = event;
      if (event.country) {
        this.countrySearchOptions.defaultValue = event.country.countryName;
        this.selectedCountryEvent(event.country);
      }
      this.clearCountryField = new String('false');
    } else {
      this.coiEntity = {};
      this.clearCountryField = new String('true');
      this.clearSFIFields();
    }
  }

  selectedCountryEvent(event) {
    if (event) {
      this.coiEntity.country = event;
      this.countrySearchOptions.defaultValue = event.countryName;
    } else {
      this.coiEntity.country = null;
    }
  }

  clearSFIFields() {
    this.coiEntity = {};
    this.additionalDetails = {
      sponsorsResearch: false
    };
    this.clearCountryField = new String('true');
    this.clearField = new String('true');
    this.isResultFromSearch = false;
  }

  checkMandatoryFilled() {
    this.mandatoryList.clear();
    if (!this.coiEntity.entityName) {
      this.mandatoryList.set('name', '* Please choose an entity name.');
    }
    if (!this.coiEntity.entityId) {
      if (!this.coiEntity.country) {
        this.mandatoryList.set('country', '* Please choose a country.');
      }
      if (!this.coiEntity.entityTypeCode || this.coiEntity.entityTypeCode === 'null') {
        this.mandatoryList.set('type', '* Please choose an entity type.');
      }
      if (!this.coiEntity.emailAddress) {
        this.mandatoryList.set('email', '* Please enter a email address.');
      }
      if (!this.coiEntity.address) {
        this.mandatoryList.set('address', '* Please enter an address.');
      }
      if (!this.coiEntity.phone) {
        this.mandatoryList.set('phone', '* Please enter phone number.');
      }
      this.emailValidation();
      if (!this.coiEntity.zipCode) {
        this.mandatoryList.set('zipCode', '* Please enter a zipCode.');
      }
    }
    if (!this.additionalDetails.involvementStartDate) {
      this.mandatoryList.set('date', '* Please enter a start date.');
    }
    if (!this.additionalDetails.staffInvolvement) {
      this.mandatoryList.set('staff', '* Please enter Relationship with Entity details.');
    }
    if (!this.additionalDetails.studentInvolvement) {
      this.mandatoryList.set('student', '* Please enter Principle Business Area of Entity details.');
    }
    if (!this.additionalDetails.instituteResourceInvolvement) {
      this.mandatoryList.set('resource', '* Please enter Relationship of Entity to your University responsibilities details.');
    }
    return this.mandatoryList.size !== 0 || this.emailWarningMsg ? false : true;

  }

  emailValidation() {
    this.emailWarningMsg = null;
    if (this.coiEntity.emailAddress) {
      this.coiEntity.emailAddress = this.coiEntity.emailAddress.trim();
      if (this.coiEntity.emailAddress !== undefined && this.coiEntity.emailAddress !== '') {
        const email = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)| (".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        if (!(email.test(String(this.coiEntity.emailAddress).toLowerCase()))) {
          this.emailWarningMsg = 'Please select a valid email address.';
        } else {
          this.emailWarningMsg = null;
        }
      }
    }
  }

  inputRestriction(event: any) {
    const pattern = /[0-9\+\-\/\ ]/;
    if (!pattern.test(String.fromCharCode(event.charCode))) {
      event.preventDefault();
    }
  }

}

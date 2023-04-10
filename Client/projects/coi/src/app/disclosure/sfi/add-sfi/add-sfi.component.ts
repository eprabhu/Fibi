import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { ActivityService } from '../../../../../../fibi/src/app/agreement/agreement-shared/activity-track/activity.service';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';
import { SfiService } from '../../../disclosure/sfi/sfi.service';
import { environment } from '../../../../environments/environment';
import { CommonService } from '../../../common/services/common.service';
import { CoiEntity, CoiFinancialEntity } from '../add-sfi.interface';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../../../fibi/src/app/common/services/end-point.config';
import { hideModal } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../../../../fibi/src/app/common/utilities/date-utilities';

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
  deployMap = environment.deployUrl;
  @ViewChild('sfiNavOverlay', { static: true }) sfiNavOverlay: ElementRef;
  isAddAttachment = false;
  isAddAssignee = false;
  dateTime: string;
  isReadMore: false;
  showRelationshipModal = false;
  clearField: any = false;
  EntitySearchOptions: any = {};
  entityDetails: CoiFinancialEntity = new CoiFinancialEntity();
  countrySearchOptions: EndpointOptions;
  clearCountryField: any;
  $subscriptions: Subscription[] = [];
  mandatoryList = new Map();
  emailWarningMsg: any;

  constructor(public sfiService: SfiService, public _commonService: CommonService,
              private _activatedRoute: ActivatedRoute, private _router: Router) { }

  ngOnInit() {
    this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
      this.entityDetails.coiEntityId = params['entityId'];
      if (this.entityDetails.coiEntityId) {
        this.getSfiDetails();
      }
    }));
    this.showSfiNavBar();
    this.EntitySearchOptions = getEndPointOptionsForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry();
  }

  getSfiDetails() {
    this.$subscriptions.push(this.sfiService.getSFIDetails(this.entityDetails.coiEntityId).subscribe(data => {
      
    }));
  }

  removeEntityId() {
    this._router.navigate([], {
      queryParams: {entityId: null},
      queryParamsHandling: 'merge'
    })
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

 createSFI() {
    if (this.checkMandatoryFilled() && !this.isSaving) {
      this.isSaving = true;
      this.entityDetails.involvementStartDate = parseDateWithoutTimestamp(this.entityDetails.involvementStartDate);
      this.entityDetails.involvementEndDate = parseDateWithoutTimestamp(this.entityDetails.involvementEndDate);
      this.$subscriptions.push(this.sfiService.createSFI({ 'coiFinancialEntity': this.entityDetails,
            disclosureId: parseInt(this._activatedRoute.snapshot.queryParamMap.get('dId')),
            proposalDisclosureWithNoSfi : this._activatedRoute.snapshot.queryParamMap.get('isSFINotAvailable') == 'true' ? true : false
      })
        .subscribe((data: any) => {
          this.sfiService.sfiDetails = JSON.parse(JSON.stringify(data));
          this.isSaving = false;
          this.showRelationshipModal= true;
          this._router.navigate([], {
            queryParams: {
              entityId: data.coiFinancialEntity.coiFinancialEntityId
            },
            queryParamsHandling: 'merge',
          });
          this.entityDetails = data.coiFinancialEntity;
          this.EntitySearchOptions.defaultValue = this.entityDetails.coiEntity.coiEntityName;
          this.entityDetails.involvementStartDate = getDateObjectFromTimeStamp(this.entityDetails.involvementStartDate);
          this.entityDetails.involvementEndDate = getDateObjectFromTimeStamp(this.entityDetails.involvementEndDate);

        },
          err => {
            this.isSaving = false;
          }));
    }
  }

  selectNewEntity(event) {
    this.entityDetails.coiEntity.coiEntityName = event.searchString;
  }

  selectedEvent(event) {
    if (event) {
      this.entityDetails.coiEntity = event;
      this.entityDetails.coiEntityId = event.coiEntityId;
      if (event.country) {
        this.countrySearchOptions.defaultValue = event.country.countryName;
        this.selectedCountryEvent(event.country);
      }
      this.clearCountryField = new String('false');
    } else {
      this.entityDetails.coiEntity = new CoiEntity();
      this.clearCountryField = new String('true');
      this.clearSFIFields();
    }
  }

  selectedCountryEvent(event) {
    this.entityDetails.coiEntity.country = event;
    this.entityDetails.coiEntity.countryCode = event.countryCode;
  }

  clearSFIFields() {
    this.entityDetails = new CoiFinancialEntity();
    this.clearCountryField = new String('true');
    this.clearField = new String('true');
  }

  checkMandatoryFilled() {
    this.mandatoryList.clear();
    if (!this.entityDetails.coiEntity.coiEntityName) {
      this.mandatoryList.set('name', '* Please choose a entity name.');
    }
    if (!this.entityDetails.coiEntityId) {
      if (!this.entityDetails.coiEntity.country) {
        this.mandatoryList.set('country', '* Please choose a country.');
      }
    }
    return this.mandatoryList.size !== 0 || this.emailWarningMsg ? false : true;
  }

  emailValidation() {
    this.emailWarningMsg = null;
    if (this.entityDetails.coiEntity.emailAddress) {
      this.entityDetails.coiEntity.emailAddress = this.entityDetails.coiEntity.emailAddress.trim();
      if (this.entityDetails.coiEntity.emailAddress !== undefined && this.entityDetails.coiEntity.emailAddress !== '') {
        const email = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)| (".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        if (!(email.test(String(this.entityDetails.coiEntity.emailAddress).toLowerCase()))) {
          this.emailWarningMsg = 'Please select a valid email address.';
        } else {
          this.emailWarningMsg = null;
        }
      }
    }
  }

}

import {Component, ElementRef, OnDestroy, OnInit, ViewChild} from '@angular/core';
import { openCoiSlider } from '../../common/utilities/custom-utilities';
import { Router } from '@angular/router';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityDataStoreService } from '../entity-data-store.service';
import { EntityDetails } from '../shared/entity-interface';
import { AutoSaveService } from '../../common/services/auto-save.service';
import { getCurrentTimeStamp, getDuration, getTimeInterval } from '../../common/utilities/date-utilities';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";

@Component({
  selector: 'app-header-details',
  templateUrl: './header-details.component.html',
  styleUrls: ['./header-details.component.scss']
})
export class HeaderDetailsComponent implements OnInit, OnDestroy {

    @ViewChild('mainEntityHeaders', { static: true }) mainEntityHeaders: ElementRef;

    sliderElementId: any;
    isShowOptions = false;
    showSlider = false;
    isShowNavBarOverlay = false;
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    entityFullAddress: string = '';
    latestPriorName: any;
    isEditMode = false;

    constructor(public router: Router, public dataStore: EntityDataStoreService, public autoSaveService: AutoSaveService) {}
    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    viewSlider(event) {
        this.showSlider = event;
        this.sliderElementId = 'duns-match-slider';
        setTimeout(() => {
            openCoiSlider(this.sliderElementId);
        });
    }

    validateSliderClose() {
        setTimeout(() => {
            this.showSlider = false;
            this.sliderElementId = '';
		}, 500);
	  }

    onClickMenuBar() {
        const NAV_ELEMENT = document.getElementById('responsive-nav');
        const IS_MENU_SHOW = NAV_ELEMENT.classList.contains('show-menu');
        const IS_SCREEN = window.innerWidth <= 1300;

        if (IS_MENU_SHOW) {
            NAV_ELEMENT.classList.remove('show-menu');
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = false;
            }
        } else {
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = true;
            }
            NAV_ELEMENT.classList.toggle('show-menu', IS_SCREEN);
        }
    }

    private getDataFromStore() {
        const entityData = this.dataStore.getData();
        if (!entityData || isEmptyObject(entityData)) { return; }
        this.entityDetails = entityData.entityDetails;
        this.latestPriorName = entityData?.priorNames[0]?.priorNames;
        this.getEntityFullAddress();
        this.isEditMode = this.dataStore.getEditMode();
    }

    getEntityFullAddress() {
        let address = this.entityDetails?.primaryAddressLine1;
        if (this.entityDetails?.primaryAddressLine2) {
            address = address + ' , ' + this.entityDetails?.primaryAddressLine2;
        }
        if(this.entityDetails?.city) {
            address = address + ' , ' + this.entityDetails?.city;
        }
        if(this.entityDetails?.state) {
            address = address + ' , ' + this.entityDetails?.state;
        }
        if(this.entityDetails?.country?.countryName) {
            address = address + ' , ' + this.entityDetails?.country?.countryName;
        }
        this.entityFullAddress = address;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }
    getTimeInterval() {
        let timeString = '';
        const startDate = this.autoSaveService.lastSavedTimeStamp;
        const END_DATE = getCurrentTimeStamp();
        if (startDate <= END_DATE) {
            const seconds = Math.floor((END_DATE - (startDate)) / 1000);
            const minutes = Math.floor(seconds / 60);
            const hours = Math.floor(minutes / 60);
            const DATE_OBJ = getDuration(startDate, END_DATE);
            const days = Math.floor(hours / 24);
            if (days > 0) {
                timeString = this.getTimeIntervalInDays(timeString, DATE_OBJ);
            }
            if (days === 0) {
                timeString = this.getTimeIntervalInHours(timeString, hours, minutes, days, seconds);
            }
        }
        return timeString ? 'Saved ' + timeString + 'ago..' : 'Saved Just Now..';
    }

    private getTimeIntervalInDays(timeString, DATE_OBJ) {
        timeString = timeString.concat(DATE_OBJ.durInYears !== 0 ? DATE_OBJ.durInYears + ' year(s) ' : '');
        timeString = timeString.concat(DATE_OBJ.durInMonths !== 0 ? DATE_OBJ.durInMonths + ' month(s) ' : '');
        timeString = timeString.concat(DATE_OBJ.durInDays !== 0 ? DATE_OBJ.durInDays + ' day(s) ' : '');
        return timeString;
    }

    private getTimeIntervalInHours(timeString, hours, minutes, days, seconds) {
        hours = hours - (days * 24);
        minutes = minutes - (days * 24 * 60) - (hours * 60);
        seconds = seconds - (days * 24 * 60 * 60) - (hours * 60 * 60) - (minutes * 60);
        timeString = timeString.concat(hours !== 0 ? hours + ' hr(s) ' : '');
        timeString = timeString.concat(hours === 0 && minutes !== 0 ? minutes + ' min(s) ' : '');
        timeString = timeString.concat(hours === 0 && minutes === 0 && seconds !== 0 ? seconds + ' sec(s) ' : '');
        return timeString;
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }
}

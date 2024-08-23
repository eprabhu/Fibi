import { Component, Input, OnInit } from '@angular/core';

@Component({
    selector: 'app-entity-sponsor-details',
    templateUrl: './entity-sponsor-details.component.html',
    styleUrls: ['./entity-sponsor-details.component.scss']
})
export class EntitySponsorDetailsComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;
    notificationTemplates = [];
    templateOptions = 'EMPTY#EMPTY#false#false';
    defaultTemplateLabel = '';

    constructor() { }

    ngOnInit() {
    }
}

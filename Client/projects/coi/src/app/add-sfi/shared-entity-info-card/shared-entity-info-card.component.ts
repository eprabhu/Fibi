import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Component({
  selector: 'app-shared-entity-info-card',
  templateUrl: './shared-entity-info-card.component.html',
  styleUrls: ['./shared-entity-info-card.component.scss']
})
export class SharedEntityInfoCardComponent implements OnInit {

  @Input() entityDetails: any = {};
  @Input() viewMore = false;
  @Input() isShowRisk = true;
  @Output() emitEntityId = new EventEmitter<any>();
  isReadMore: false;
  constructor(public commonServices: CommonService) { }

  ngOnInit() {
  }

  public viewEntityDetails(entityId: any) {
     this.emitEntityId.emit(entityId);
  }

  getWarningClass(typeCode): string {
    switch (typeCode) {
        case '1':
            return 'invalid';
        case '2':
            return 'medium-risk';
        case '3':
            return 'low-risk';
        default:
            return;
    }
}

}

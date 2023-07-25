import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'app-shared-entity-info-card',
  templateUrl: './shared-entity-info-card.component.html',
  styleUrls: ['./shared-entity-info-card.component.scss']
})
export class SharedEntityInfoCardComponent implements OnInit {

  @Input() entityDetails: any = {};
  @Input() viewMore = false;
  @Output() emitEntityId = new EventEmitter<any>();
  constructor() { }

  ngOnInit() {
  }


  public viewEntityDetails(entityId :any){
     this.emitEntityId.emit(entityId);
  }
}

import { animate, state, style, transition, trigger, group, keyframes, query, stagger } from '@angular/animations';

export const slideInOut = trigger('slideInOut', [
  state('in', style({ height: '*', opacity: 0 })),
  transition(':leave', [
    style({ height: '*', opacity: 1 }),
    group([
      animate(300, style({ height: 0 })),
      animate('200ms ease-in-out', style({ 'opacity': '0' }))
    ])
  ]),
  transition(':enter', [
    style({ height: '0', opacity: 0 }),
    group([
      animate(300, style({ height: '*' })),
      animate('400ms ease-in-out', style({ 'opacity': '1' }))
    ])
  ])
]);

export const slowSlideInOut = trigger('slowSlideInOut', [
  state('in', style({ height: '*', opacity: 0 })),
  transition(':leave', [
    style({ height: '*', opacity: 1 }),
    group([
      animate(800, style({ height: 0 })),
      animate('600ms ease-in-out', style({ 'opacity': '0' }))
    ])
  ]),
  transition(':enter', [
    style({ height: '0', opacity: 0 }),
    group([
      animate(800, style({ height: '*' })),
      animate('800ms ease-in-out', style({ 'opacity': '1' }))
    ])
  ])
]);
export const fadeDown = trigger('fadeDown', [
  transition(':enter', [
    animate('1s ease-in', keyframes([
      style({ opacity: 0, transform: 'translateY(-30%)', offset: 0 }),
      style({ opacity: .5, transform: 'translateY(5px)', offset: 0.3 }),
      style({ opacity: 1, transform: 'translateY(0)', offset: 1.0 }),
    ]))
  ]),
  transition(':leave', [
    animate('1s ease-in', keyframes([
      style({ opacity: 1, transform: 'translateY(0)', offset: 0 }),
      style({ opacity: .5, transform: 'translateY(5px)', offset: 0.3 }),
      style({ opacity: 0, transform: 'translateY(-30%)', offset: 1.0 }),
    ]))
  ])
]);
export const easeInOUt =  trigger('items', [
  transition(':enter', [
    style({ transform: 'translateX(100%)', opacity: 0 }),
    animate('400ms', style({ transform: 'translateX(0)', opacity: 1 }))
  ]),
  transition(':leave', [
    style({ transform: 'translateX(0)', opacity: 1 }),
    animate('400ms', style({ transform: 'translateX(100%)', opacity: 0 }))
  ])
]);
export const itemAnim = trigger('itemAnim', [
  transition(':enter', [
      animate('1s ease-in', keyframes([
          style({ opacity: 0, transform: 'translateY(-30%)', offset: 0 }),
          style({ opacity: .5, transform: 'translateY(5px)', offset: 0.3 }),
          style({ opacity: 1, transform: 'translateY(0)', offset: 1.0 }),
      ]))
  ])
]);

export const slideHorizontal = trigger(
    'enterAnimation', [
    transition(':enter', [
      style({ transform: 'translateX(100%)', opacity: 0 }),
      animate('1200ms', style({ transform: 'translateX(0)', opacity: 1 }))
    ]),
    transition(':leave', [
      style({ transform: 'translateX(0)', opacity: 1 }),
      animate('1500ms', style({ transform: 'translateX(100%)', opacity: 0 }))
    ])
  ]);

  export const slideHorizontalOverlay = trigger(
    'enterAnimation', [
    transition(':enter', [
      style({ transform: 'translateX(100%)', opacity: 0 }),
      animate('800ms', style({ transform: 'translateX(0)', opacity: 1 }))
    ]),
    transition(':leave', [
      style({ transform: 'translateX(0)', opacity: 1 }),
      animate('1000ms', style({ transform: 'translateX(100%)', opacity: 0 }))
    ])
  ]);

  export const slideHorizontalFast = trigger(
    'enterAnimation', [
    transition(':enter', [
      style({ transform: 'translateX(100%)', opacity: 0 }),
      animate('600ms', style({ transform: 'translateX(0)', opacity: 1 }))
    ]),
    transition(':leave', [
      style({ transform: 'translateX(0)', opacity: 1 }),
      animate('700ms', style({ transform: 'translateX(100%)', opacity: 0 }))
    ])
  ]);

  export const easeIn =  trigger('items', [
    transition(':enter', [
      animate('1s ease-in', keyframes([
        style({opacity: 0, transform: 'translateY(-30%)', offset: 0}),
        style({opacity: .5, transform: 'translateY(5px)',  offset: 0.3}),
        style({opacity: 1, transform: 'translateY(0)',     offset: 1.0}),
      ]))
    ])
]);


export const fadeIn =  trigger('fadeIn', [
  transition(':enter', [
    animate('400ms ease-in', keyframes([
      style({opacity: 0,  offset: 0}),
      style({opacity: .5, offset: 0.5}),
      style({opacity: 1, offset: 1.0}),
    ]))
  ])
]);

export const listAnimation = trigger('listAnimation', [
  transition('* => *', [
    query(':enter', [
      style({ opacity: 0, transform: 'translateY(-10px)' }),
      stagger('100ms', [
        animate('400ms cubic-bezier(0.35, 0, 0.25, 1)',
        style({ opacity: 1, transform: 'translateY(0)' }))
      ])], { optional: true }
    )
  ])
]);

export const leftSlideInOut = trigger('leftSlideInOut', [
  transition(':enter', [
    animate('300ms ease-in-out', keyframes([
      style({ opacity: 0, transform: 'translateX(-20px)', offset: 0 }),
      style({ opacity: .3, transform: 'translateX(-10px)', offset: 0.3 }),
      style({ opacity: 1, transform: 'translateX(0)', offset: 1.0 }),
    ]))
  ]),
  transition(':leave', [
    animate('300ms ease-in-out', keyframes([
      style({ opacity: 1, transform: 'translateX(0)', offset: 0 }),
      style({ opacity: .3, transform: 'translateX(-5px)', offset: 0.3 }),
      style({ opacity: 0, transform: 'translateX(-10px)', offset: 1.0 }),
    ]))
  ])
]);

export const topSlideInOut = trigger('topSlideInOut', [
  transition(':enter', [
    style({ height: '0', opacity: 0 }),
    group([
      animate(200, style({ height: '*' })),
      animate('300ms ease-in-out', style({ 'opacity': '1' }))
    ])
  ]),
  transition(':leave', [
    animate('250ms ease-in-out', keyframes([
      style({ opacity: 1, height: '*', transform: 'translateY(0)', offset: 0 }),
      style({ opacity: .3, transform: 'translateY(-5px)', offset: 0.3 }),
      style({ opacity: 0, height: 0, transform: 'translateY(-10px)', offset: 1.0 }),
    ]))
  ])
]);

export const fadeInOutHeight  = trigger('fadeInOutHeight', [
  transition(':enter', [
    style({ height: '0', opacity: 0 }),
    group([
      animate('400ms ease-in-out', style({ 'opacity': '1' }))
    ])
  ]),
  transition(':leave', [
    style({ height: '*', opacity: 1 }),
    group([
      animate('200ms ease-in-out', style({ 'opacity': '0' }))
    ])
  ])
]);

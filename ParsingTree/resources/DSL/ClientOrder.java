// ClientOrder.java
package org.dg.biz
public class ClientOrder {
  public enum BuySell {
    BUY,
    SELL
  }
  private String accountNo;

  private List<LineItem> lineItems = new ArrayList<LineItem>();

  // constructors, getters ..
}

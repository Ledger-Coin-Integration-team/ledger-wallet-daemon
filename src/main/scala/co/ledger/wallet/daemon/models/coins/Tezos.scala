package co.ledger.wallet.daemon.models.coins

import java.util.Date

import co.ledger.core
import co.ledger.core.implicits._
import co.ledger.wallet.daemon.async.MDCPropagatingExecutionContext.Implicits.global
import co.ledger.wallet.daemon.configurations.DaemonConfiguration
import co.ledger.wallet.daemon.models.coins.Coin._
import co.ledger.wallet.daemon.utils.HexUtils
import com.fasterxml.jackson.annotation.JsonProperty

import scala.collection.JavaConverters._
import scala.concurrent.Future

object Tezos {
  val currencyFamily = core.WalletType.TEZOS

  def newNetworkParamsView(currencyName: String, from: core.TezosLikeNetworkParameters): NetworkParamsView = {
    TezosNetworkParamsView(
      from.getIdentifier,
      from.getMessagePrefix,
      HexUtils.valueOf(from.getXPUBVersion),
      HexUtils.valueOf(from.getImplicitPrefix),
      HexUtils.valueOf(from.getOriginatedPrefix),
      from.getAdditionalTIPs,
      from.getTimestampDelay,
    )
  }

  // TODO: correct models below this line

  def newTransactionView(from: core.TezosLikeTransaction): TransactionView = {
    TezosTransactionView(
      Option(from.getBlock).map (newBlockView),
      Option(from.getFees).map (fees => fees.toString),
      from.getHash,
      from.getTime,
      from.getInputs.asScala.map(newInputView),
      from.getLockTime,
      from.getOutputs.asScala.map(newOutputView)
    )
  }

  private def newBlockView(from: core.TezosLikeBlock): BlockView = {
    CommonBlockView(from.getHash, from.getHeight, from.getTime)
  }
}

case class TezosNetworkParamsView(
                                     @JsonProperty("identifier") identifier: String,
                                     @JsonProperty("message_prefix") messagePrefix: String,
                                     @JsonProperty("xpub_version") xpubVersion: String,
                                     @JsonProperty("implicit_prefix") implicitPrefix: String,
                                     @JsonProperty("originated_prefix") originatedPrefix: String,
                                     @JsonProperty("additional_tips") additionalTips: String,
                                     @JsonProperty("timestamp_delay") timestampDelay: BigInt
                                   ) extends NetworkParamsView

object TezosNetworkParamsView {
  def apply(n: TezosLikeNetworkParameters): TezosNetworkParamsView =
    TezosNetworkParamsView(
      n.getIdentifier,
      HexUtils.valueOf(n.getMessagePrefix),
      HexUtils.valueOf(n.getVersion),
      HexUtils.valueOf(n.getImplicitPrefix),
      HexUtils.valueOf(n.getOriginatedPrefix),
      n.getAdditionalTIPs.asScala.toList,
      n.getTimestampDelay
    )
}

case class TezosTransactionView(
                                   @JsonProperty("type") operationType: TezosOperationTag,
                                   @JsonProperty("hash") hash: String,
                                   @JsonProperty("fees") fees: Option[String],
                                   @JsonProperty("receiver") receiver: Option[String],
                                   @JsonProperty("sender") sender: String,
                                   @JsonProperty("value") value: String,
                                   @JsonProperty("date") date: Date,
                                   @JsonProperty("signing_pubkey") signing_pubkey: String,
                                   @JsonProperty("counter") counter: BigInt,
                                   @JsonProperty("gas_limit") gasLimit: BigInt,
                                   @JsonProperty("storage_limit") storageLimit: BigInt,
                                   @JsonProperty("block_hash") blockHash: BigInt,
                                   @JsonProperty("status") status: Int,
                                 ) extends TransactionView

object TezosTransactionView {
  def apply(op: Operation): TezosTransactionView = {
    val tx = op.asTezosLikeOperation().getTransaction

    TezosTransactionView(
      tx.getType,
      tx.getHash,
      tx.getFees.toString,
      tx.getReceiver.toString,
      tx.getSender.toString,
      tx.getValue.toString,
      tx.getDate,
      HexUtils.valueOf(tx.getSigningPubkey),
      tx.getCounter,
      tx.getGasLimit.toString,
      tx.getStorageLimit,
      tx.getBlockHash,
      tx.getStatus
    )
  }
}

case class UnsignedTezosTransactionView(
                            @JsonProperty("raw_transaction") rawTransaction: String) extends TransactionView

object UnsignedTezosTransactionView {
  def apply(tx: core.TezosLikeTransaction): UnsignedTezosTransactionView = {
    UnsignedTezosTransactionView(
      tx.serialize().toString()
    )
  }
}
